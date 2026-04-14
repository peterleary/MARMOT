<script>
  import { open } from "@tauri-apps/plugin-shell";
  import { MARMOT_VERSION } from "../stores/constants.js";

  let {
    status = "Starting up...",
    missing = [],
    onInstallQuarto = null,
    installingQuarto = false,
    installStatus = "",
  } = $props();
  let blocked = $derived(missing.length > 0);
</script>

<div class="splash">
  <div class="splash-content">
    <img src="/marmot-logo.png" alt="MARMOT" class="splash-logo" />
    <div class="splash-title">MARMOT</div>
    <div class="splash-subtitle">Flow Cytometry Analysis</div>

    {#if blocked}
      <div class="blocked-box">
        <div class="blocked-heading">Required software not found</div>
        <div class="blocked-text">
          MARMOT needs the following to run. Please install, then relaunch the app.
        </div>
        <div class="missing-list">
          {#each missing as dep}
            <div class="missing-item">
              <span class="missing-x">&#10007;</span>
              <div class="missing-info">
                {#if dep.name === "Quarto" && dep.installable && onInstallQuarto}
                  <span class="missing-name">{dep.name}</span>
                  <span class="missing-desc">{dep.description}</span>
                  {#if installingQuarto}
                    <div class="install-progress">
                      <span class="mini-spinner"></span>
                      <span class="install-status-text">{installStatus}</span>
                    </div>
                  {:else if installStatus && !installingQuarto}
                    <span class="install-error-text">{installStatus}</span>
                    <button class="missing-link" onclick={() => open(dep.url)}>
                      Install manually
                    </button>
                  {:else}
                    <button class="install-btn" onclick={onInstallQuarto}>
                      Install
                    </button>
                  {/if}
                {:else}
                  <button class="missing-link" onclick={() => open(dep.url)}>
                    {dep.name}
                  </button>
                  <span class="missing-desc">{dep.description}</span>
                {/if}
              </div>
            </div>
          {/each}
        </div>
      </div>
    {:else}
      <div class="spinner-row">
        <span class="dot"></span>
        <span class="dot"></span>
        <span class="dot"></span>
      </div>
      <div class="splash-status">{status}</div>
    {/if}
  </div>

  <div class="splash-version">v{MARMOT_VERSION}</div>
</div>

<style>
  .splash {
    position: fixed;
    inset: 0;
    background: linear-gradient(135deg, #0f172a 0%, #1e293b 60%, #334155 100%);
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    z-index: 1000;
  }

  .splash-content {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.6rem;
  }

  .splash-logo {
    width: 72px;
    height: 72px;
    border-radius: 14px;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.5);
    margin-bottom: 0.4rem;
  }

  .splash-title {
    font-size: 2rem;
    font-weight: 800;
    letter-spacing: 0.18em;
    color: #dbeafe;
  }

  .splash-subtitle {
    font-size: 0.82rem;
    color: #94a3b8;
    letter-spacing: 0.04em;
    margin-top: -0.3rem;
  }

  /* ── blocked state ── */
  .blocked-box {
    margin-top: 1.6rem;
    background: rgba(255, 255, 255, 0.05);
    border: 1px solid rgba(255, 255, 255, 0.1);
    border-radius: 10px;
    padding: 1.2rem 1.6rem;
    max-width: 380px;
    text-align: center;
  }

  .blocked-heading {
    font-size: 0.9rem;
    font-weight: 700;
    color: #fbbf24;
    margin-bottom: 0.4rem;
  }

  .blocked-text {
    font-size: 0.75rem;
    color: #94a3b8;
    line-height: 1.45;
    margin-bottom: 1rem;
  }

  .missing-list {
    display: flex;
    flex-direction: column;
    gap: 0.65rem;
    text-align: left;
  }

  .missing-item {
    display: flex;
    align-items: flex-start;
    gap: 0.5rem;
  }

  .missing-x {
    color: #ef4444;
    font-size: 0.85rem;
    font-weight: 700;
    margin-top: 0.05rem;
    flex-shrink: 0;
  }

  .missing-info {
    display: flex;
    flex-direction: column;
    gap: 0.1rem;
  }

  .missing-link {
    background: none;
    border: none;
    padding: 0;
    color: #60a5fa;
    font-size: 0.82rem;
    font-weight: 600;
    cursor: pointer;
    text-align: left;
    font-family: inherit;
    text-decoration: underline;
    text-decoration-color: rgba(96, 165, 250, 0.4);
    text-underline-offset: 2px;
  }
  .missing-link:hover {
    color: #93bbfd;
  }

  .missing-name {
    font-size: 0.82rem;
    font-weight: 600;
    color: #e2e8f0;
  }

  .missing-desc {
    font-size: 0.7rem;
    color: #64748b;
  }

  .install-btn {
    margin-top: 0.3rem;
    padding: 0.3rem 0.85rem;
    background: #2563eb;
    color: #fff;
    border: none;
    border-radius: 5px;
    font-size: 0.78rem;
    font-weight: 600;
    font-family: inherit;
    cursor: pointer;
    transition: background 0.15s;
    align-self: flex-start;
  }
  .install-btn:hover {
    background: #1d4ed8;
  }

  .install-progress {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    margin-top: 0.3rem;
  }

  .mini-spinner {
    width: 12px;
    height: 12px;
    border: 2px solid rgba(96, 165, 250, 0.3);
    border-top-color: #60a5fa;
    border-radius: 50%;
    animation: spin 0.8s linear infinite;
    flex-shrink: 0;
  }

  @keyframes spin {
    to { transform: rotate(360deg); }
  }

  .install-status-text {
    font-size: 0.7rem;
    color: #94a3b8;
    line-height: 1.3;
  }

  .install-error-text {
    font-size: 0.7rem;
    color: #f87171;
    margin-top: 0.2rem;
  }

  /* ── loading state ── */
  .spinner-row {
    display: flex;
    gap: 0.45rem;
    margin-top: 1.4rem;
  }

  .dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: #3b82f6;
    animation: bounce 1.2s ease-in-out infinite;
  }
  .dot:nth-child(2) { animation-delay: 0.2s; }
  .dot:nth-child(3) { animation-delay: 0.4s; }

  @keyframes bounce {
    0%, 80%, 100% { transform: scale(0.7); opacity: 0.4; }
    40%            { transform: scale(1);   opacity: 1;   }
  }

  .splash-status {
    font-size: 0.75rem;
    color: #64748b;
    margin-top: 0.5rem;
    letter-spacing: 0.02em;
    min-height: 1.1em;
  }

  .splash-version {
    position: absolute;
    bottom: 1.2rem;
    font-size: 0.65rem;
    color: #334155;
    font-family: "SF Mono", "Fira Code", monospace;
    letter-spacing: 0.05em;
  }
</style>
