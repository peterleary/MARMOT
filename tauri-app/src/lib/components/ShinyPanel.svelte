<script>
  import { invoke } from "@tauri-apps/api/core";
  import { open } from "@tauri-apps/plugin-dialog";
  import { pipelineOutputDir, rscriptPath } from "../stores/pipeline.js";

  let previousResultsDir = $state("");
  let shinyLaunching = $state(false);

  async function launch(rFilesPath) {
    shinyLaunching = true;
    try {
      await invoke("launch_shiny_app", {
        rscriptPath: $rscriptPath,
        rFilesPath,
      });
      setTimeout(() => { shinyLaunching = false; }, 8000);
    } catch (e) {
      shinyLaunching = false;
      alert("Could not launch Shiny app: " + e);
    }
  }

  async function handleOpenPipelineShiny() {
    await launch($pipelineOutputDir + "/R_files");
  }

  async function handleOpenPreviousShiny() {
    if (!previousResultsDir) return;
    await launch(previousResultsDir + "/R_files");
  }

  async function handleBrowsePrevious() {
    const selected = await open({ directory: true, title: "Select Results_Files_* folder" });
    if (selected) previousResultsDir = selected;
  }
</script>

<div class="shiny-panel">
  <h2 class="panel-title">Shiny App Launcher</h2>

  <!-- From current run -->
  <div class="section-card">
    <h3 class="section-heading">From current run</h3>
    {#if $pipelineOutputDir}
      <p class="section-desc">
        Pipeline output: <code class="path-label">{$pipelineOutputDir}</code>
      </p>
      <button
        class="btn btn-shiny"
        onclick={handleOpenPipelineShiny}
        disabled={shinyLaunching}
      >
        {shinyLaunching ? "Starting…" : "Open Shiny App"}
      </button>
    {:else}
      <p class="section-desc muted">
        No pipeline results available. Run the pipeline first, or load previous results below.
      </p>
      <button class="btn btn-shiny" disabled>Open Shiny App</button>
    {/if}
  </div>

  <!-- Load previous results -->
  <div class="section-card">
    <h3 class="section-heading">Load previous results</h3>
    <p class="section-desc">Browse to a <code>Results_Files_*</code> folder from a previous run.</p>
    <div class="input-row">
      <input
        type="text"
        class="path-input"
        bind:value={previousResultsDir}
        placeholder="Select a Results_Files_* folder…"
        readonly
      />
      <button class="btn btn-secondary" onclick={handleBrowsePrevious}>Browse</button>
    </div>
    <button
      class="btn btn-shiny"
      onclick={handleOpenPreviousShiny}
      disabled={!previousResultsDir || shinyLaunching}
    >
      {shinyLaunching ? "Starting…" : "Open in Shiny"}
    </button>
  </div>

  {#if shinyLaunching}
    <div class="launch-notice">
      Starting R and Shiny — this may take a few seconds…
    </div>
  {/if}
</div>

<style>
  .shiny-panel {
    padding: 1.25rem 1.5rem;
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }
  .panel-title {
    font-size: 1rem;
    font-weight: 700;
    color: #1e293b;
    margin: 0 0 0.25rem;
    letter-spacing: 0.02em;
  }
  .section-card {
    background: #f0f5ff;
    border: 1px solid #dbeafe;
    border-radius: 8px;
    padding: 1rem 1.1rem;
    display: flex;
    flex-direction: column;
    gap: 0.55rem;
  }
  .section-heading {
    font-size: 0.85rem;
    font-weight: 600;
    color: #1e293b;
    margin: 0;
    letter-spacing: 0.01em;
  }
  .section-desc {
    font-size: 0.78rem;
    color: #475569;
    margin: 0;
    line-height: 1.5;
  }
  .section-desc.muted {
    color: #94a3b8;
    font-style: italic;
  }
  .path-label {
    font-family: "SF Mono", "Fira Code", "Consolas", monospace;
    font-size: 0.72rem;
    background: #e2e8f0;
    padding: 0.1rem 0.35rem;
    border-radius: 3px;
    color: #334155;
    word-break: break-all;
  }
  .input-row {
    display: flex;
    gap: 0.5rem;
    align-items: center;
  }
  .path-input {
    flex: 1;
    padding: 0.28rem 0.55rem;
    border: 1px solid #cbd5e1;
    border-radius: 5px;
    font-size: 0.78rem;
    font-family: inherit;
    background: #fafbff;
    color: #374151;
  }
  .btn {
    padding: 0.35rem 0.9rem;
    border-radius: 5px;
    cursor: pointer;
    font-size: 0.82rem;
    font-weight: 500;
    font-family: inherit;
    border: 1px solid;
    transition: background 0.15s;
    white-space: nowrap;
  }
  .btn-shiny {
    background: #16a34a;
    color: #fff;
    border-color: #15803d;
    align-self: flex-start;
  }
  .btn-shiny:hover:not(:disabled) { background: #15803d; }
  .btn-shiny:disabled {
    background: #86efac;
    border-color: #86efac;
    cursor: not-allowed;
    color: #fff;
  }
  .btn-secondary {
    background: #fff;
    color: #374151;
    border-color: #d1d5db;
  }
  .btn-secondary:hover { background: #f3f4f6; }
  .launch-notice {
    font-size: 0.78rem;
    color: #ca8a04;
    background: #fefce8;
    border: 1px solid #fde68a;
    border-radius: 5px;
    padding: 0.4rem 0.75rem;
  }
</style>
