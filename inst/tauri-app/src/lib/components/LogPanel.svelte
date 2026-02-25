<script>
  import { invoke } from "@tauri-apps/api/core";
  import { open } from "@tauri-apps/plugin-dialog";
  import { pipelineState, logLines, startTime, pipelineOutputDir, pipelineHtmlPath, rscriptPath }
      from "../stores/pipeline.js";

  let previousResultsDir = $state("");

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
    try {
      await invoke("launch_shiny_app", {
        rscriptPath: $rscriptPath,
        rFilesPath: $pipelineOutputDir + "/R_files",
      });
    } catch (e) {
      console.error("Failed to launch Shiny app:", e);
      alert("Could not launch Shiny app: " + e);
    }
  }

  async function handleViewReport() {
    try {
      await invoke("open_path", { path: $pipelineHtmlPath });
    } catch (e) {
      alert("Could not open report: " + e);
    }
  }

  async function handleOpenFolder() {
    try {
      await invoke("open_path", { path: $pipelineOutputDir });
    } catch (e) {
      alert("Could not open folder: " + e);
    }
  }

  async function handleBrowsePrevious() {
    const selected = await open({ directory: true, title: "Select Results_Files_* folder" });
    if (selected) previousResultsDir = selected;
  }

  async function handleOpenPreviousShiny() {
    if (!previousResultsDir) return;
    try {
      await invoke("launch_shiny_app", {
        rscriptPath: $rscriptPath,
        rFilesPath: previousResultsDir + "/R_files",
      });
    } catch (e) {
      alert("Could not launch Shiny app: " + e);
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
    <div class="post-run-actions">
      <span class="actions-label">Next steps:</span>
      <button class="btn-action btn-shiny" onclick={handleOpenShiny}>
        Open Shiny App
      </button>
      <button class="btn-action btn-secondary" onclick={handleViewReport}>
        View Report
      </button>
      <button class="btn-action btn-secondary" onclick={handleOpenFolder}>
        Open Folder
      </button>
    </div>
  {/if}
  <!-- Load previous results -->
  <div class="prev-results">
    <span class="prev-results-label">Load previous results</span>
    <input
      type="text"
      class="prev-results-input"
      bind:value={previousResultsDir}
      placeholder="Select a Results_Files_* folder..."
      readonly
    />
    <button class="btn-action btn-secondary" onclick={handleBrowsePrevious}>Browse</button>
    <button
      class="btn-action btn-shiny"
      onclick={handleOpenPreviousShiny}
      disabled={!previousResultsDir}
    >
      Open in Shiny
    </button>
  </div>

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
  .post-run-actions {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.4rem 0;
    border-top: 1px solid #e5e7eb;
    margin-bottom: 0.5rem;
    flex-wrap: wrap;
  }
  .actions-label {
    font-size: 0.78rem;
    color: #6b7280;
    font-weight: 500;
    white-space: nowrap;
  }
  .btn-action {
    padding: 0.3rem 0.75rem;
    border-radius: 4px;
    cursor: pointer;
    font-size: 0.82rem;
    font-weight: 500;
    font-family: inherit;
    border: 1px solid;
    transition: background 0.15s;
  }
  .btn-shiny {
    background: #16a34a;
    color: #fff;
    border-color: #15803d;
  }
  .btn-shiny:hover:not(:disabled) { background: #15803d; }
  .btn-shiny:disabled { background: #86efac; border-color: #86efac; cursor: not-allowed; }
  .btn-secondary {
    background: #fff;
    color: #374151;
    border-color: #d1d5db;
  }
  .btn-secondary:hover { background: #f3f4f6; }
  .prev-results {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.4rem 0;
    border-top: 1px solid #e5e7eb;
    margin-bottom: 0.5rem;
    flex-wrap: wrap;
  }
  .prev-results-label {
    font-size: 0.78rem;
    color: #6b7280;
    font-weight: 500;
    white-space: nowrap;
  }
  .prev-results-input {
    flex: 1;
    min-width: 160px;
    padding: 0.25rem 0.5rem;
    border: 1px solid #d1d5db;
    border-radius: 4px;
    font-size: 0.78rem;
    font-family: inherit;
    background: #fafafa;
    color: #374151;
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
