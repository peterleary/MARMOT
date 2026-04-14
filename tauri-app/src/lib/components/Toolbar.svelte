<script>
  import { invoke } from "@tauri-apps/api/core";
  import { open, save } from "@tauri-apps/plugin-dialog";
  import { metadata, isDirty, fcsFolder, runName } from "../stores/metadata.js";
  import { pipelineState, rscriptPath, quartoPath, launchPipeline,
           pipelineOutputDir, pipelineHtmlPath } from "../stores/pipeline.js";
  import { validateSettings, validateFileData } from "../utils/validation.js";
  import { scanFcsFolder } from "../utils/fcs-scan.js";

  let { onActiveTab = () => {} } = $props();

  // Truncate long paths for the toolbar display — keep the last two segments
  // so users see the meaningful filename/folder name. Full path is available
  // via the tooltip.
  function shortenPath(p) {
    if (!p) return "";
    const sep = p.includes("/") ? "/" : "\\";
    const parts = p.split(sep).filter(Boolean);
    if (parts.length <= 2) return p;
    return "…" + sep + parts.slice(-2).join(sep);
  }

  async function handleOpen() {
    const selected = await open({
      title: "Open MARMOT Metadata",
      filters: [{ name: "Excel", extensions: ["xlsx"] }],
    });
    if (!selected) return;

    try {
      const data = await invoke("read_excel", { path: selected });
      metadata.set(data);
      isDirty.set(false);

      // Auto-fill FCS folder with the Excel file's parent directory
      const sep = selected.includes("/") ? "/" : "\\";
      const parent = selected.substring(0, selected.lastIndexOf(sep));
      if (parent) fcsFolder.set(parent);
    } catch (e) {
      alert("Failed to open file: " + (e.message || e));
    }
  }

  async function handleSave() {
    let path = $metadata.path;
    if (!path) {
      path = await save({
        title: "Save MARMOT Metadata",
        filters: [{ name: "Excel", extensions: ["xlsx"] }],
        defaultPath: "MARMOT_Metadata.xlsx",
      });
      if (!path) return;
    }

    try {
      await invoke("write_excel", { metadata: $metadata, path });
      metadata.update((m) => ({ ...m, path }));
      isDirty.set(false);
    } catch (e) {
      alert("Failed to save file: " + (e.message || e));
    }
  }

  async function handleNew() {
    if ($isDirty && !confirm("You have unsaved changes. Create new metadata?")) return;
    try {
      const data = await invoke("create_new_metadata");
      metadata.set(data);
      isDirty.set(false);
    } catch (e) {
      alert("Failed to create new metadata: " + (e.message || e));
    }
  }

  async function handleBrowse() {
    const selected = await open({ directory: true, title: "Select FCS folder" });
    if (!selected) return;
    try {
      // Full scan: sets fcsFolder, enumerates files into file_data,
      // peeks the first file for markers and pre-fills Study Data.
      // Result is a small status object — we surface it via alert for now
      // because the toolbar has no dedicated banner slot.
      const result = await scanFcsFolder(selected);
      if (result && result.kind === "warn") {
        alert(result.text);
      }
    } catch (e) {
      alert("Failed to scan FCS folder: " + (e.message || e));
    }
  }

  async function handleRun() {
    // Validate
    const settingsErrors = validateSettings($metadata.pipeline_settings);
    const fileErrors = validateFileData($metadata.file_data);
    const allErrors = [...settingsErrors, ...fileErrors];

    if (allErrors.length > 0) {
      alert("Validation errors:\n\n" + allErrors.join("\n"));
      return;
    }

    if (!$fcsFolder) {
      alert("Please select an FCS folder first.");
      return;
    }

    if (!$rscriptPath) {
      alert("Rscript not found. Please check R installation.");
      return;
    }

    if (!$quartoPath) {
      alert("Quarto not found. Install it from https://quarto.org/docs/get-started/");
      return;
    }

    // Capture folder and runName before async operations (stores may change)
    const folder = $fcsFolder;
    const name = $runName;

    // Save metadata to temp location
    const metadataPath = folder + "/" + name.replace(/[^A-Za-z0-9]/g, "_") + "_metadata.xlsx";
    try {
      await invoke("write_excel", { metadata: $metadata, path: metadataPath });
    } catch (e) {
      alert("Failed to save metadata: " + (e.message || e));
      return;
    }

    // Clear stale post-run state before starting a new run
    pipelineOutputDir.set(null);
    pipelineHtmlPath.set(null);

    // Switch to log tab BEFORE launching (listeners live in the store, not here)
    onActiveTab("log");

    // Launch pipeline — event listeners are managed in the store module
    // so they survive this component being unmounted by the tab switch.
    await launchPipeline({
      rscriptPath: $rscriptPath,
      metadataPath,
      runName: name,
      fcsFolder: folder,
    });
  }
</script>

<div class="toolbar">
  <div class="toolbar-left">
    <span class="toolbar-group-label">Input</span>
    <button class="btn" onclick={handleNew} title="Start from a blank metadata">New</button>
    <button
      class="btn"
      onclick={handleOpen}
      title="Open an existing MARMOT_Metadata.xlsx file"
    >Open Metadata…</button>
    <button
      class="btn"
      onclick={handleBrowse}
      title="Point MARMOT at a folder of .fcs files. Enumerates files, fills in file_name / sample_id, and peeks the first file to pre-fill the marker panel."
    >Open FCS Folder…</button>
    <button class="btn" onclick={handleSave} title="Save metadata Excel">Save{$isDirty ? " *" : ""}</button>

    {#if $metadata.path || $fcsFolder}
      <div class="separator"></div>
      <div class="loaded-display" title={$metadata.path || $fcsFolder}>
        {#if $metadata.path}
          <span class="loaded-icon">📄</span>
          <span class="loaded-path">{shortenPath($metadata.path)}</span>
        {:else}
          <span class="loaded-icon">📁</span>
          <span class="loaded-path">{shortenPath($fcsFolder)}</span>
        {/if}
      </div>
    {/if}
  </div>

  <div class="toolbar-right">
    <div class="run-name-input">
      <label for="run-name">Run Name:</label>
      <input id="run-name" type="text" bind:value={$runName} />
    </div>
    <button
      class="btn btn-run"
      onclick={handleRun}
      disabled={$pipelineState === "running"}
    >
      {$pipelineState === "running" ? "Running..." : "Run Pipeline"}
    </button>
  </div>
</div>

<style>
  .toolbar {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem 0.75rem;
    background: #f0f5ff;
    border-bottom: 1px solid #dbeafe;
    gap: 1rem;
    flex-wrap: wrap;
  }
  .toolbar-left, .toolbar-right {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }
  .toolbar-group-label {
    font-size: 0.72rem;
    font-weight: 600;
    color: #94a3b8;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    margin-right: 0.1rem;
    user-select: none;
  }
  .btn {
    padding: 0.35rem 0.7rem;
    border: 1px solid #bfdbfe;
    border-radius: 4px;
    background: #fff;
    cursor: pointer;
    font-size: 0.82rem;
    color: #333;
    font-family: inherit;
  }
  .btn:hover:not(:disabled) {
    background: #dbeafe;
    border-color: #2563eb;
  }
  .btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
  .btn-run {
    background: #2563eb;
    color: #fff;
    border-color: #1d4ed8;
    font-weight: 600;
    padding: 0.4rem 1rem;
  }
  .btn-run:hover:not(:disabled) {
    background: #1d4ed8;
  }
  .separator {
    width: 1px;
    height: 24px;
    background: #ccc;
    margin: 0 0.3rem;
  }
  .run-name-input {
    display: flex;
    align-items: center;
    gap: 0.3rem;
  }
  .run-name-input label {
    font-size: 0.8rem;
    color: #555;
    white-space: nowrap;
  }
  .run-name-input input {
    padding: 0.3rem 0.5rem;
    border: 1px solid #ccc;
    border-radius: 4px;
    font-size: 0.8rem;
    font-family: inherit;
    width: 160px;
  }
  .loaded-display {
    display: flex;
    align-items: center;
    gap: 0.35rem;
    padding: 0.3rem 0.6rem;
    background: #fafafa;
    border: 1px solid #e2e8f0;
    border-radius: 4px;
    font-size: 0.78rem;
    color: #334155;
    max-width: 320px;
    cursor: default;
  }
  .loaded-icon {
    font-size: 0.9rem;
    line-height: 1;
  }
  .loaded-path {
    font-family: "SF Mono", "Fira Code", "Consolas", monospace;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
</style>
