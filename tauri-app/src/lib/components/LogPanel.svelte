<script>
  import { invoke } from "@tauri-apps/api/core";
  import { pipelineState, logLines, startTime, pipelineProgress,
           pipelineOutputDir, pipelineHtmlPath, rscriptPath }
      from "../stores/pipeline.js";

  let shinyLaunching = $state(false);

  let logContainer = $state(null);
  let elapsed = $state("0:00");
  let timer = $state(null);

  // Per-chunk "still going" tracking. Resets every time Quarto moves to a
  // new chunk. If we sit on the same chunk for > 60s, show a reassuring
  // message that rotates every 90s so the user can tell the GUI is alive.
  let chunkStartTime = $state(null);
  let secondsOnChunk = $state(0);

  // Per-chunk custom messages. Chunks not listed fall back to GENERIC_MESSAGES.
  const CHUNK_MESSAGES = {
    dim_reduction: [
      "Running dimensionality reduction — UMAP and TSNE can take 5–10 minutes on 100k+ cells.",
      "TSNE is O(n log n) with big constants. This is the pipeline's slowest chunk.",
      "Marmot is burrowing through nearest neighbours... 🦫",
      "Still crunching. PaCMAP, UMAP and TSNE each build their own kNN index."
    ],
    parc_clustering: [
      "PARC is building a kNN graph + running Leiden community detection.",
      "kNN construction is the slow part here. We're getting there.",
      "Still clustering. PARC scales well but big datasets still take a few minutes."
    ],
    mparc_clustering: [
      "Running the pure-R PARC fallback — slower than the Python version, be patient.",
      "Mparc is the R implementation. It works, it's just not fast."
    ],
    FlowSOM: [
      "FlowSOM is training its self-organising map and running metaclustering.",
      "Still clustering. FlowSOM is usually quick, but bigger datasets take longer."
    ],
    import_fcs_load: [
      "Loading FCS files into memory — disk speed matters here.",
      "Still reading FCS. Large files with many samples take a while."
    ],
    run_peacoqc: [
      "PeacoQC is scanning every event in every file for bad signal regions.",
      "QC in progress. This scales with (total cells × channels)."
    ],
    run_flow_auto_qc: [
      "flowAI is running quality control on every file.",
      "Still doing QC. Large files take longer."
    ],
    da_ds_analysis: [
      "Fitting differential abundance / state models — one GLM per cluster per contrast.",
      "Still running DA/DS. Scales with (clusters × contrasts)."
    ],
    create_sce: [
      "Building the SingleCellExperiment object and its assay matrices.",
      "Still assembling the SCE. Large datasets mean big assay matrices."
    ]
  };

  const GENERIC_MESSAGES = [
    "Still working... 🦫",
    "Hang tight, this chunk is taking a bit.",
    "Keep an eye on the log below for updates."
  ];

  // Compute the current "still going" message from the chunk name and how
  // long we've been on it. Returns null when the chunk just started (< 60s),
  // so no banner appears for fast chunks.
  let stillGoingMessage = $derived.by(() => {
    if (!$pipelineProgress || secondsOnChunk < 60) return null;
    const chunk = $pipelineProgress.chunk;
    const messages = (chunk && CHUNK_MESSAGES[chunk]) || GENERIC_MESSAGES;
    // Rotate every 90s after the 60s threshold
    const idx = Math.floor((secondsOnChunk - 60) / 90) % messages.length;
    return messages[idx];
  });

  // Reset chunk timer whenever Quarto moves to a new chunk
  $effect(() => {
    const key = $pipelineProgress
      ? `${$pipelineProgress.done}-${$pipelineProgress.chunk}`
      : null;
    if (key !== null) {
      chunkStartTime = Date.now();
      secondsOnChunk = 0;
    } else {
      chunkStartTime = null;
      secondsOnChunk = 0;
    }
  });

  // Auto-scroll log to bottom
  $effect(() => {
    if ($logLines && logContainer) {
      logContainer.scrollTop = logContainer.scrollHeight;
    }
  });

  // Elapsed time + per-chunk time updater
  $effect(() => {
    if ($pipelineState === "running" && $startTime) {
      timer = setInterval(() => {
        const now = Date.now();
        const diff = Math.floor((now - $startTime) / 1000);
        const mins = Math.floor(diff / 60);
        const secs = diff % 60;
        elapsed = `${mins}:${secs.toString().padStart(2, "0")}`;
        if (chunkStartTime) {
          secondsOnChunk = Math.floor((now - chunkStartTime) / 1000);
        }
      }, 1000);
    }
    return () => {
      if (timer) clearInterval(timer);
    };
  });

  async function handleStop() {
    try {
      await invoke("cancel_pipeline");
      pipelineState.set("cancelled");
    } catch (e) {
      console.error("Cancel failed:", e);
    }
  }

  async function handleOpenShiny() {
    shinyLaunching = true;
    try {
      await invoke("launch_shiny_app", {
        rscriptPath: $rscriptPath,
        rFilesPath: $pipelineOutputDir + "/R_files",
      });
      // Shiny app launches in a background R process; reset button after
      // a generous delay since we can't poll the Shiny port from here.
      setTimeout(() => { shinyLaunching = false; }, 15000);
    } catch (e) {
      shinyLaunching = false;
      console.error("Failed to launch Shiny app:", e);
      alert("Could not launch Shiny app: " + (e.message || e));
    }
  }

  async function handleViewReport() {
    try {
      await invoke("open_path", { path: $pipelineHtmlPath });
    } catch (e) {
      alert("Could not open report: " + (e.message || e));
    }
  }

  async function handleOpenFolder() {
    try {
      await invoke("open_path", { path: $pipelineOutputDir });
    } catch (e) {
      alert("Could not open folder: " + (e.message || e));
    }
  }


</script>

<div class="log-panel">
  <div class="log-toolbar">
    <div class="log-status">
      {#if $pipelineState === "running"}
        <span class="status-dot running"></span>
        <span>Running... ({elapsed})</span>
      {:else if $pipelineState === "done"}
        <span class="status-dot done"></span>
        <span>Completed ({elapsed})</span>
      {:else if $pipelineState === "error"}
        <span class="status-dot error"></span>
        <span>Error</span>
      {:else if $pipelineState === "cancelled"}
        <span class="status-dot cancelled"></span>
        <span>Cancelled</span>
      {:else}
        <span class="status-dot idle"></span>
        <span>Ready</span>
      {/if}
    </div>
    <div class="log-actions">
      {#if $pipelineState === "running"}
        <button class="btn-stop" onclick={handleStop}>Stop</button>
      {/if}
    </div>
  </div>
  {#if $pipelineState === "running" && $pipelineProgress}
    <div class="progress-container">
      <div class="progress-header">
        <span class="progress-chunk">
          {$pipelineProgress.chunk || "…"}
        </span>
        <span class="progress-count">
          {$pipelineProgress.done} / {$pipelineProgress.total}
        </span>
      </div>
      <div
        class="progress-track"
        role="progressbar"
        aria-valuenow={$pipelineProgress.done}
        aria-valuemin="0"
        aria-valuemax={$pipelineProgress.total}
      >
        <div
          class="progress-fill"
          style="width: {($pipelineProgress.done / $pipelineProgress.total) * 100}%"
        ></div>
      </div>
      {#if stillGoingMessage}
        {#key stillGoingMessage}
          <div class="still-going">{stillGoingMessage}</div>
        {/key}
      {/if}
    </div>
  {/if}
  {#if $pipelineState === "error"}
    <div class="error-banner">
      <span class="error-x">&#10007;</span>
      <span class="error-title">Pipeline failed — check the log below for details</span>
    </div>
  {/if}
  {#if $pipelineState === "done" && $pipelineOutputDir}
    <div class="post-run-banner">
      <div class="banner-header">
        <span class="banner-check">&#10003;</span>
        <span class="banner-title">Pipeline complete</span>
      </div>
      <div class="banner-actions">
        <button class="action-card card-shiny" onclick={handleOpenShiny} disabled={shinyLaunching}>
          <span class="card-icon">&#9881;</span>
          <span class="card-label">{shinyLaunching ? "Starting…" : "Open Shiny App"}</span>
          <span class="card-desc">Explore results interactively</span>
        </button>
        <button class="action-card card-report" onclick={handleViewReport}>
          <span class="card-icon">&#128196;</span>
          <span class="card-label">View Report</span>
          <span class="card-desc">Open the rendered HTML report</span>
        </button>
        <button class="action-card card-folder" onclick={handleOpenFolder}>
          <span class="card-icon">&#128193;</span>
          <span class="card-label">Open Folder</span>
          <span class="card-desc">Browse output files in Finder</span>
        </button>
      </div>
    </div>
  {/if}
<pre class="log-output" bind:this={logContainer}>{#each $logLines as line}{line}
{/each}</pre>
</div>

<style>
  .log-panel {
    display: flex;
    flex-direction: column;
    height: 100%;
    padding: 0.5rem 1rem;
  }
  .log-toolbar {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 0.5rem;
  }
  .log-status {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    font-size: 0.85rem;
    color: #555;
  }
  .status-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
  }
  .status-dot.running { background: #f0a500; animation: pulse 1s infinite; }
  .status-dot.done { background: #4caf50; }
  .status-dot.error { background: #e53935; }
  .status-dot.cancelled { background: #999; }
  .status-dot.idle { background: #ccc; }
  @keyframes pulse {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.4; }
  }
  .log-actions {
    display: flex;
    gap: 0.5rem;
  }
  .btn-stop {
    padding: 0.3rem 0.8rem;
    border: 1px solid #e53935;
    border-radius: 4px;
    background: #fff;
    color: #e53935;
    cursor: pointer;
    font-size: 0.82rem;
    font-weight: 500;
  }
  .btn-stop:hover {
    background: #fbe9e7;
  }
  .progress-container {
    margin-bottom: 0.6rem;
  }
  .progress-header {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    font-size: 0.78rem;
    color: #555;
    margin-bottom: 0.3rem;
    font-family: "SF Mono", "Fira Code", "Consolas", monospace;
  }
  .progress-chunk {
    font-weight: 600;
    color: #3f3f46;
  }
  .progress-count {
    color: #71717a;
    font-variant-numeric: tabular-nums;
  }
  .progress-track {
    height: 6px;
    background: #e4e4e7;
    border-radius: 3px;
    overflow: hidden;
  }
  .progress-fill {
    height: 100%;
    background: linear-gradient(90deg, #ef4444 0%, #f97316 100%);
    transition: width 0.3s ease;
    border-radius: 3px;
  }
  .still-going {
    margin-top: 0.5rem;
    padding: 0.4rem 0.6rem;
    font-size: 0.78rem;
    font-style: italic;
    color: #71717a;
    background: #fafafa;
    border-left: 3px solid #f97316;
    border-radius: 0 4px 4px 0;
    animation: still-going-fade 0.4s ease-out;
  }
  @keyframes still-going-fade {
    from { opacity: 0; transform: translateY(-2px); }
    to   { opacity: 1; transform: translateY(0); }
  }
  .post-run-banner {
    background: linear-gradient(135deg, #f0fdf4 0%, #ecfdf5 100%);
    border: 1px solid #bbf7d0;
    border-radius: 10px;
    padding: 0.75rem 1rem;
    margin-bottom: 0.6rem;
  }
  .banner-header {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    margin-bottom: 0.6rem;
  }
  .banner-check {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 22px;
    height: 22px;
    border-radius: 50%;
    background: #16a34a;
    color: #fff;
    font-size: 0.75rem;
    font-weight: 700;
  }
  .banner-title {
    font-size: 0.9rem;
    font-weight: 600;
    color: #14532d;
  }
  .banner-actions {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 0.5rem;
  }
  .action-card {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.2rem;
    padding: 0.7rem 0.5rem;
    border-radius: 8px;
    cursor: pointer;
    border: 1.5px solid;
    font-family: inherit;
    transition: transform 0.12s, box-shadow 0.12s;
  }
  .action-card:hover:not(:disabled) {
    transform: translateY(-1px);
    box-shadow: 0 3px 10px rgba(0, 0, 0, 0.08);
  }
  .card-icon {
    font-size: 1.3rem;
    line-height: 1;
  }
  .card-label {
    font-size: 0.82rem;
    font-weight: 600;
  }
  .card-desc {
    font-size: 0.68rem;
    opacity: 0.7;
    text-align: center;
  }
  .card-shiny {
    background: #16a34a;
    border-color: #15803d;
    color: #fff;
  }
  .card-shiny:hover:not(:disabled) { background: #15803d; }
  .card-shiny:disabled { background: #86efac; border-color: #86efac; cursor: not-allowed; opacity: 0.7; }
  .card-report {
    background: #2563eb;
    border-color: #1d4ed8;
    color: #fff;
  }
  .card-report:hover { background: #1d4ed8; }
  .card-folder {
    background: #f59e0b;
    border-color: #d97706;
    color: #fff;
  }
  .card-folder:hover { background: #d97706; }
  .error-banner {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    background: #fef2f2;
    border: 1px solid #fecaca;
    border-radius: 8px;
    padding: 0.6rem 1rem;
    margin-bottom: 0.5rem;
  }
  .error-x {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 22px;
    height: 22px;
    min-width: 22px;
    border-radius: 50%;
    background: #dc2626;
    color: #fff;
    font-size: 0.75rem;
    font-weight: 700;
  }
  .error-title {
    font-size: 0.88rem;
    font-weight: 600;
    color: #991b1b;
  }
  .log-output {
    flex: 1;
    background: #1e1e1e;
    color: #d4d4d4;
    padding: 0.75rem;
    border-radius: 6px;
    font-family: "SF Mono", "Fira Code", "Consolas", monospace;
    font-size: 0.78rem;
    line-height: 1.5;
    overflow-y: auto;
    margin: 0;
    min-height: 200px;
    white-space: pre-wrap;
    word-break: break-word;
  }
</style>
