<script>
  import { rscriptPath, rVersion, marmotInstalled, pipelineState } from "../stores/pipeline.js";

  let stateLabel = $derived(
    $pipelineState === "running" ? "Running..." :
    $pipelineState === "done" ? "Completed" :
    $pipelineState === "error" ? "Error" :
    $pipelineState === "cancelled" ? "Cancelled" :
    "Ready"
  );

  let stateColor = $derived(
    $pipelineState === "running" ? "#f0a500" :
    $pipelineState === "done" ? "#4caf50" :
    $pipelineState === "error" ? "#e53935" :
    "#64748b"
  );
</script>

<div class="status-bar">
  <div class="status-item">
    <span class="status-label">R</span>
    <span class="status-value">{$rscriptPath ? $rscriptPath.split('/').pop() : "not found"}</span>
    {#if $rVersion}
      <span class="status-dim">({$rVersion.replace(/^R scripting front-end version /, '')})</span>
    {/if}
  </div>
  <div class="status-sep"></div>
  <div class="status-item">
    <span class="status-label">MARMOT</span>
    {#if $marmotInstalled}
      <span class="status-ok">OK</span>
    {:else}
      <span class="status-warn">not installed</span>
    {/if}
  </div>
  <span class="attribution">made by the marmots in Switzerland&nbsp; <span class="swiss-flag" aria-label="Switzerland">+</span> 🥕 🐿️ 🏔️</span>
  <div class="status-state" style="color: {stateColor}">
    {stateLabel}
  </div>
</div>

<style>
  .status-bar {
    display: flex;
    align-items: center;
    gap: 0.8rem;
    padding: 0.3rem 0.9rem;
    background: #0f172a;
    color: #94a3b8;
    font-size: 0.72rem;
    font-family: "SF Mono", "Fira Code", "Consolas", monospace;
    border-top: 1px solid #1e293b;
    user-select: none;
  }
  .status-item {
    display: flex;
    align-items: center;
    gap: 0.3rem;
  }
  .status-label {
    color: #64748b;
    font-weight: 600;
  }
  .status-value {
    color: #cbd5e1;
  }
  .status-dim {
    color: #475569;
  }
  .status-ok {
    color: #7cb87c;
  }
  .status-warn {
    color: #f59e0b;
  }
  .status-sep {
    width: 1px;
    height: 12px;
    background: #334155;
  }
  .attribution {
    margin-left: auto;
    margin-right: auto;
    color: #334155;
    font-size: 0.66rem;
    font-style: italic;
    letter-spacing: 0.01em;
    white-space: nowrap;
  }
  /* Hand-drawn Swiss flag — Windows Segoe UI Emoji doesn't ship country flags
     (shows ISO code "CH" instead), so we paint one from CSS. Portable and
     guaranteed to render identically on every platform. */
  .swiss-flag {
    display: inline-block;
    width: 0.9em;
    height: 0.9em;
    background: #d52b1e;
    color: #ffffff;
    font-weight: 900;
    font-style: normal;
    font-size: 0.75em;
    line-height: 0.9em;
    text-align: center;
    border-radius: 1px;
    vertical-align: -1px;
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  }
  .status-state {
    font-weight: 600;
    letter-spacing: 0.02em;
  }
</style>
