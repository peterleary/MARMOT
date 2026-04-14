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
  <span class="attribution">made by the marmots in Switzerland&nbsp; 🇨🇭 🥕 🐿️ 🏔️</span>
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
  .status-state {
    font-weight: 600;
    letter-spacing: 0.02em;
  }
</style>
