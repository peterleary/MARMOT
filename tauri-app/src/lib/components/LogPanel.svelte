<script>
  import { invoke } from "@tauri-apps/api/core";
  import { pipelineState, logLines, startTime, pipelineOutputDir, pipelineHtmlPath, rscriptPath }
      from "../stores/pipeline.js";

  let shinyLaunching = $state(false);

  let logContainer = $state(null);
  let elapsed = $state("0:00");
  let timer = $state(null);

  // Auto-scroll log to bottom
  $effect(() => {
    if ($logLines && logContainer) {
      logContainer.scrollTop = logContainer.scrollHeight;
    }
  });

  // Elapsed time updater
  $effect(() => {
    if ($pipelineState === "running" && $startTime) {
      timer = setInterval(() => {
        const diff = Math.floor((Date.now() - $startTime) / 1000);
        const mins = Math.floor(diff / 60);
        const secs = diff % 60;
        elapsed = `${mins}:${secs.toString().padStart(2, "0")}`;
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
