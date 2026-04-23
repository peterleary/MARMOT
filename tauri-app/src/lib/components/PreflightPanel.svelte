<script>
  let { report = null, checking = false, onclose = () => {} } = $props();

  let expanded = $state({});
  function toggle(id) { expanded[id] = !expanded[id]; }

  // Whenever a new report arrives, auto-expand every fail row that has
  // details so the user sees the reason without an extra click. We track
  // the last report identity by reference so users can still collapse a
  // row manually (re-running the check re-expands fails).
  let lastReport = null;
  $effect(() => {
    if (report && report !== lastReport) {
      lastReport = report;
      const next = {};
      for (const c of report.checks) {
        if (c.status === "fail" && c.details.length > 0) next[c.id] = true;
      }
      expanded = next;
    }
  });

  function icon(status) {
    if (status === "pass") return "✓";
    if (status === "warn") return "⚠";
    if (status === "fail") return "✗";
    return "–"; // skip
  }

  const tierLabels = {
    1: "Metadata structure",
    2: "Filesystem",
    3: "FCS content spot-check",
  };

  let grouped = $derived.by(() => {
    if (!report) return [];
    const byTier = new Map();
    for (const c of report.checks) {
      if (!byTier.has(c.tier)) byTier.set(c.tier, []);
      byTier.get(c.tier).push(c);
    }
    return [...byTier.entries()].sort((a, b) => a[0] - b[0]);
  });
</script>

<div class="preflight-panel">
  <div class="panel-header">
    <span class="panel-title">Pre-flight Check</span>
    <button class="close-btn" onclick={onclose} title="Close">×</button>
  </div>

  {#if checking}
    <div class="checking">
      <span class="spinner"></span>
      <span>Running checks…</span>
    </div>
  {:else if !report}
    <div class="empty">No report yet — click <strong>Pre-flight check</strong> to run.</div>
  {:else}
    {@const s = report.summary}
    {@const ok = s.failures === 0}
    <div class="summary" class:ok class:bad={s.failures > 0} class:warn={ok && s.warnings > 0}>
      <span class="summary-icon">{ok ? (s.warnings > 0 ? "⚠" : "✓") : "✗"}</span>
      <span class="summary-text">
        {s.passed} passed
        {#if s.warnings > 0}· {s.warnings} warning{s.warnings === 1 ? "" : "s"}{/if}
        {#if s.failures > 0}· <strong>{s.failures} failure{s.failures === 1 ? "" : "s"}</strong>{/if}
        {#if s.skipped > 0}· {s.skipped} skipped{/if}
      </span>
    </div>

    {#each grouped as [tier, checks]}
      <div class="tier-group">
        <div class="tier-label">Tier {tier} — {tierLabels[tier] || ""}</div>
        {#each checks as c}
          <div class="check-row status-{c.status}" class:clickable={c.details.length > 0}>
            <!-- svelte-ignore a11y_no_noninteractive_tabindex -->
            <div
              class="check-main"
              role={c.details.length > 0 ? "button" : undefined}
              tabindex={c.details.length > 0 ? 0 : undefined}
              onclick={() => c.details.length > 0 && toggle(c.id)}
              onkeydown={(e) => { if (c.details.length > 0 && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); toggle(c.id); } }}
            >
              <span class="check-icon">{icon(c.status)}</span>
              <div class="check-body">
                <div class="check-label">{c.label}</div>
                <div class="check-message">{c.message}</div>
              </div>
              {#if c.details.length > 0}
                <span class="caret" class:open={expanded[c.id]}>▸</span>
              {/if}
            </div>
            {#if expanded[c.id] && c.details.length > 0}
              <ul class="details">
                {#each c.details as d}
                  <li>{d}</li>
                {/each}
              </ul>
            {/if}
          </div>
        {/each}
      </div>
    {/each}
  {/if}
</div>

<style>
  .preflight-panel {
    background: #fff;
    border: 1px solid #e2e8f0;
    border-radius: 6px;
    box-shadow: 0 8px 24px rgba(0, 0, 0, 0.08);
    padding: 0;
    font-size: 0.84rem;
    max-height: 70vh;
    overflow-y: auto;
    width: min(780px, calc(100vw - 1.5rem));
    min-width: min(420px, calc(100vw - 1.5rem));
  }
  .panel-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.55rem 0.85rem;
    background: #f8fafc;
    border-bottom: 1px solid #e2e8f0;
    border-radius: 6px 6px 0 0;
    position: sticky;
    top: 0;
  }
  .panel-title {
    font-weight: 600;
    color: #1e293b;
  }
  .close-btn {
    background: transparent;
    border: none;
    font-size: 1.2rem;
    line-height: 1;
    cursor: pointer;
    color: #64748b;
    padding: 0 0.3rem;
  }
  .close-btn:hover { color: #1e293b; }

  .checking, .empty {
    padding: 1.2rem;
    text-align: center;
    color: #64748b;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 0.5rem;
  }
  .spinner {
    width: 14px;
    height: 14px;
    border: 2px solid #dbeafe;
    border-top-color: #2563eb;
    border-radius: 50%;
    animation: spin 0.8s linear infinite;
    display: inline-block;
  }
  @keyframes spin { to { transform: rotate(360deg); } }

  .summary {
    margin: 0.6rem 0.85rem;
    padding: 0.5rem 0.7rem;
    border-radius: 5px;
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-weight: 500;
  }
  .summary.ok    { background: #dcfce7; color: #166534; }
  .summary.warn  { background: #fef3c7; color: #92400e; }
  .summary.bad   { background: #fee2e2; color: #991b1b; }
  .summary-icon {
    font-size: 1.1rem;
    font-weight: 700;
  }

  .tier-group {
    padding: 0 0.85rem 0.5rem;
  }
  .tier-label {
    font-size: 0.72rem;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: #94a3b8;
    font-weight: 600;
    margin: 0.5rem 0 0.25rem;
  }
  .check-row {
    border: 1px solid #e2e8f0;
    border-radius: 4px;
    margin-bottom: 0.3rem;
    overflow: hidden;
  }
  .check-row.status-pass { border-left: 3px solid #16a34a; }
  .check-row.status-warn { border-left: 3px solid #d97706; background: #fffbeb; }
  .check-row.status-fail { border-left: 3px solid #dc2626; background: #fef2f2; }
  .check-row.status-skip { border-left: 3px solid #cbd5e1; opacity: 0.7; }

  .check-main {
    display: flex;
    align-items: center;
    gap: 0.55rem;
    padding: 0.45rem 0.6rem;
    width: 100%;
    cursor: default;
    color: #1e293b;
  }
  .check-body {
    flex: 1 1 auto;
    min-width: 0;
    display: flex;
    flex-direction: column;
    gap: 0.05rem;
  }
  .check-row.clickable .check-main { cursor: pointer; }
  .check-row.clickable .check-main:hover { background: rgba(0, 0, 0, 0.03); }

  .check-icon {
    font-weight: 700;
    text-align: center;
    width: 22px;
    flex-shrink: 0;
  }
  .status-pass .check-icon { color: #16a34a; }
  .status-warn .check-icon { color: #d97706; }
  .status-fail .check-icon { color: #dc2626; }
  .status-skip .check-icon { color: #94a3b8; }
  .check-label {
    font-weight: 500;
    word-break: break-word;
  }
  .check-message {
    color: #64748b;
    font-size: 0.78rem;
    text-align: left;
    word-break: break-word;
  }
  .caret {
    color: #94a3b8;
    font-size: 0.72rem;
    transition: transform 0.12s;
    flex-shrink: 0;
  }
  .caret.open { transform: rotate(90deg); }

  .details {
    list-style: none;
    padding: 0.35rem 0.85rem 0.5rem 2.3rem;
    margin: 0;
    background: rgba(0, 0, 0, 0.015);
    border-top: 1px dashed #e2e8f0;
  }
  .details li {
    padding: 0.15rem 0;
    color: #475569;
    font-family: "SF Mono", "Fira Code", Consolas, monospace;
    font-size: 0.76rem;
    word-break: break-all;
  }
</style>
