<script>
  // Extension for annotated cells: name the clusters of a finished run, then
  // redraw its report with figures and DA/DS for the named populations,
  // without recomputing QC, clustering or embeddings.
  import { invoke } from "@tauri-apps/api/core";
  import { open } from "@tauri-apps/plugin-dialog";
  import {
    pipelineOutputDir, pipelineState, rscriptPath, quartoPath, launchPipeline,
  } from "../stores/pipeline.js";

  let { onActiveTab = () => {} } = $props();

  let resultsDir = $state($pipelineOutputDir || "");
  let summary = $state(null);
  let summaryError = $state("");
  let runName = $state("Annotated populations");
  let busy = $state(false);
  let explorerStarting = $state(false);

  // Only a Results_Files_* folder (not its R_files) holds the labels sheet
  function resultsRoot(p) {
    return p.replace(/[\\/]R_files[\\/]?$/, "");
  }

  async function refresh() {
    summary = null;
    summaryError = "";
    if (!resultsDir) return;
    try {
      summary = await invoke("read_extension_summary", { resultsDir: resultsRoot(resultsDir) });
    } catch (e) {
      summaryError = String(e.message || e);
    }
  }

  $effect(() => {
    resultsDir;
    refresh();
  });

  async function browse() {
    const selected = await open({ directory: true, title: "Select a Results_Files_* folder" });
    if (selected) resultsDir = resultsRoot(selected);
  }

  async function annotate() {
    if (!$rscriptPath) { alert("Rscript not found. Please check the R installation."); return; }
    explorerStarting = true;
    try {
      await invoke("launch_shiny_app", { rscriptPath: $rscriptPath, rFilesPath: resultsRoot(resultsDir) + "/R_files" });
      setTimeout(() => { explorerStarting = false; }, 8000);
    } catch (e) {
      explorerStarting = false;
      alert("Could not open the Explorer: " + (e.message || e));
    }
  }

  async function openSheet() {
    if (summary?.labels_path) await invoke("open_path", { path: summary.labels_path }).catch(() => {});
  }

  async function runExtension() {
    if (!$rscriptPath) { alert("Rscript not found. Please check the R installation."); return; }
    if (!$quartoPath) { alert("Quarto not found. Install it from https://quarto.org/docs/get-started/"); return; }
    if ($pipelineState === "running") { alert("MARMOT is already running. Wait for it to finish first."); return; }
    if (!summary?.metadata_path) { alert("These results hold no copy of their metadata sheet, so the extension cannot start from them."); return; }
    busy = true;
    try {
      // The first run's own metadata, with RDataFolder pointing at its results
      const md = await invoke("read_excel", { path: summary.metadata_path });
      const dir = resultsRoot(resultsDir);
      const idx = md.pipeline_settings.findIndex((s) => s.variable === "RDataFolder");
      if (idx >= 0) md.pipeline_settings[idx] = { ...md.pipeline_settings[idx], setting: dir };
      else md.pipeline_settings.push({ variable: "RDataFolder", setting: dir, info: "" });

      const parent = summary.parent_dir;
      const metadataPath = parent + "/" + runName.replace(/[^A-Za-z0-9]/g, "_") + "_extension_metadata.xlsx";
      await invoke("write_excel", { metadata: md, path: metadataPath });

      onActiveTab("log");
      await launchPipeline({ rscriptPath: $rscriptPath, metadataPath, runName, fcsFolder: parent });
    } catch (e) {
      alert("Could not start the extension: " + (e.message || e));
    } finally {
      busy = false;
    }
  }

  let ready = $derived(!!summary && summary.is_results && summary.n_named + summary.n_dropped > 0);
</script>

<div class="ext-panel">
  <h2 class="panel-title">Extension for annotated cells</h2>
  <p class="intro">
    Name the clusters of a finished run, then let the marmots redraw its report with figures and
    DA/DS for your named populations. Nothing is clustered again: QC, clusters and maps stay
    exactly as they were. Clusters with the same name merge into one population, a cluster left
    unnamed stays as it is, and <code>drop</code> leaves out a small, clearly noisy cluster.
  </p>

  <div class="section-card">
    <h3 class="section-heading">1. Choose the results</h3>
    <div class="input-row">
      <input type="text" class="path-input" bind:value={resultsDir} placeholder="Select a Results_Files_* folder…" readonly />
      <button class="btn btn-secondary" onclick={browse}>Browse</button>
    </div>
    {#if summaryError}
      <p class="section-desc warn">{summaryError}</p>
    {:else if summary && !summary.is_results}
      <p class="section-desc warn">This folder does not look like MARMOT results (it has no R_files folder).</p>
    {/if}
  </div>

  <div class="section-card">
    <h3 class="section-heading">2. Name the clusters</h3>
    {#if summary?.labels_path}
      <p class="section-desc status" class:ok={ready}>
        {#if summary.n_named + summary.n_dropped === 0}
          No clusters named yet ({summary.n_clusters} clusters).
        {:else}
          {summary.n_named} of {summary.n_clusters} clusters named, in {summary.n_populations}
          population{summary.n_populations === 1 ? "" : "s"}{summary.n_dropped ? `; ${summary.n_dropped} dropped` : ""}.
        {/if}
      </p>
    {:else if summary?.is_results}
      <p class="section-desc warn">These results have no MARMOT_Cluster_Labels.xlsx. Results from before MARMOT 2.0 cannot be extended; run MARMOT again to get one.</p>
    {/if}
    <p class="section-desc">
      Name them in the Explorer (Relabel Clusters, then "Save for the MARMOT extension"), or type the
      names straight into the Label column of <code>MARMOT_Cluster_Labels.xlsx</code>.
    </p>
    <div class="input-row">
      <button class="btn btn-shiny" onclick={annotate} disabled={!summary?.is_results || explorerStarting}>
        {explorerStarting ? "Starting…" : "Annotate in the Explorer"}
      </button>
      <button class="btn btn-secondary" onclick={openSheet} disabled={!summary?.labels_path}>Open the sheet</button>
      <button class="btn btn-secondary" onclick={refresh} disabled={!resultsDir}>Refresh</button>
    </div>
  </div>

  <div class="section-card">
    <h3 class="section-heading">3. Run the extension</h3>
    <div class="input-row">
      <label for="ext-name" class="section-desc">Run name</label>
      <input id="ext-name" type="text" class="path-input" bind:value={runName} />
    </div>
    {#if summary?.parent_dir}
      <p class="section-desc">The new report goes next to the first one, in <code class="path-label">{summary.parent_dir}</code>.</p>
    {/if}
    <button class="btn btn-run" onclick={runExtension} disabled={!ready || busy || $pipelineState === "running"}>
      {busy ? "Starting…" : "Run extension"}
    </button>
    {#if summary && !ready && summary.labels_path}
      <p class="section-desc muted">Name at least one cluster first.</p>
    {/if}
  </div>
</div>

<style>
  .ext-panel { padding: 1.25rem 1.5rem; display: flex; flex-direction: column; gap: 1rem; }
  .panel-title { font-size: 1rem; font-weight: 700; color: #1e293b; margin: 0; letter-spacing: 0.02em; }
  .intro { font-size: 0.8rem; color: #475569; margin: 0; line-height: 1.55; max-width: 60rem; }
  .section-card {
    background: #f0f5ff; border: 1px solid #dbeafe; border-radius: 8px;
    padding: 1rem 1.1rem; display: flex; flex-direction: column; gap: 0.55rem;
  }
  .section-heading { font-size: 0.85rem; font-weight: 600; color: #1e293b; margin: 0; }
  .section-desc { font-size: 0.78rem; color: #475569; margin: 0; line-height: 1.5; }
  .section-desc.muted { color: #94a3b8; font-style: italic; }
  .section-desc.warn { color: #b45309; }
  .section-desc.status { font-weight: 600; color: #334155; }
  .section-desc.status.ok { color: #15803d; }
  .path-label {
    font-family: "SF Mono", "Fira Code", "Consolas", monospace; font-size: 0.72rem;
    background: #e2e8f0; padding: 0.1rem 0.35rem; border-radius: 3px; color: #334155; word-break: break-all;
  }
  .input-row { display: flex; gap: 0.5rem; align-items: center; flex-wrap: wrap; }
  .path-input {
    flex: 1; min-width: 12rem; padding: 0.28rem 0.55rem; border: 1px solid #cbd5e1; border-radius: 5px;
    font-size: 0.78rem; font-family: inherit; background: #fafbff; color: #374151;
  }
  .btn {
    padding: 0.35rem 0.9rem; border-radius: 5px; cursor: pointer; font-size: 0.82rem; font-weight: 500;
    font-family: inherit; border: 1px solid; transition: background 0.15s; white-space: nowrap;
  }
  .btn:disabled { opacity: 0.5; cursor: not-allowed; }
  .btn-shiny { background: #16a34a; color: #fff; border-color: #15803d; }
  .btn-shiny:hover:not(:disabled) { background: #15803d; }
  .btn-run { background: #dc2626; color: #fff; border-color: #b91c1c; align-self: flex-start; }
  .btn-run:hover:not(:disabled) { background: #b91c1c; }
  .btn-secondary { background: #fff; color: #374151; border-color: #d1d5db; }
  .btn-secondary:hover:not(:disabled) { background: #f3f4f6; }
</style>
