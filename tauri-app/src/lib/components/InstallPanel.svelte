<script>
  import { invoke } from "@tauri-apps/api/core";
  import { listen } from "@tauri-apps/api/event";
  import { rscriptPath, installState, installLines, installStartTime, packageStatus } from "../stores/pipeline.js";

  let logContainer = $state(null);
  let includeSuggests = $state(true);
  let includePython = $state(true);
  let elapsed = $state("0:00");
  let timer = $state(null);
  let runningCmd = $state(null); // "check" | "install" | null

  // Auto-scroll log
  $effect(() => {
    if ($installLines && logContainer) logContainer.scrollTop = logContainer.scrollHeight;
  });

  // Elapsed timer
  $effect(() => {
    if ($installState === "running" && $installStartTime) {
      timer = setInterval(() => {
        const diff = Math.floor((Date.now() - $installStartTime) / 1000);
        elapsed = `${Math.floor(diff / 60)}:${String(diff % 60).padStart(2, "0")}`;
      }, 1000);
    }
    return () => {
      if (timer) clearInterval(timer);
    };
  });

  async function runCommand(cmd, invokeArgs) {
    if (!$rscriptPath) {
      alert("Rscript not found. Please ensure R is installed.");
      return;
    }
    installLines.set([]);
    installState.set("running");
    installStartTime.set(Date.now());
    runningCmd = cmd;

    const unlistenLog = await listen("install-log", (e) => {
      installLines.update((l) => [...l, e.payload]);
    });
    const unlistenDone = await listen("install-done", async (e) => {
      installState.set(e.payload.success ? "done" : "error");
      runningCmd = null;
      unlistenLog();
      unlistenDone();
      // Refresh package availability so Settings tab updates immediately
      try {
        const status = await invoke("query_installed_packages", { rscriptPath: $rscriptPath });
        packageStatus.set(status);
      } catch (_) {}
    });

    try {
      await invokeArgs();
    } catch (e) {
      installLines.update((l) => [...l, "ERROR: " + e]);
      installState.set("error");
      runningCmd = null;
      unlistenLog();
      unlistenDone();
    }
  }

  async function handleCheckSetup() {
    await runCommand("check", () =>
      invoke("run_check_setup", { rscriptPath: $rscriptPath })
    );
  }

  async function handleInstall() {
    await runCommand("install", () =>
      invoke("run_install", {
        rscriptPath: $rscriptPath,
        includeSuggests,
        includePython,
      })
    );
  }

  let isRunning = $derived($installState === "running");
</script>

<div class="setup-panel">

  <div class="welcome">
    <h2>Welcome to MARMOT</h2>
    <p>Before your first analysis, check that your R environment has everything it needs.</p>
  </div>

  <!-- Primary action: Check Setup -->
  <div class="check-card">
    <div class="check-card-label">Start here</div>
    <div class="check-card-body">
      <div class="check-card-text">
        <strong>Check your setup</strong>
        <span>Scans your R environment and reports which packages are installed and which are missing.</span>
      </div>
      <button
        class="btn-check"
        onclick={handleCheckSetup}
        disabled={isRunning}
      >
        {#if isRunning && runningCmd === "check"}
          Checking... ({elapsed})
        {:else}
          &#9654; Check Setup
        {/if}
      </button>
    </div>
  </div>

  <!-- Divider -->
  <div class="section-divider">
    <span>Missing packages? Install them below</span>
  </div>

  <!-- Secondary action: Install -->
  <div class="install-section">
    <div class="install-options">
      <label class="option-row">
        <input type="checkbox" bind:checked={includeSuggests} disabled={isRunning} />
        <span>
          <strong>Include optional packages</strong>
          <small>Seurat</small>
        </span>
      </label>
      <label class="option-row">
        <input type="checkbox" bind:checked={includePython} disabled={isRunning} />
        <span>
          <strong>Set up Python environment</strong>
          <small>PARC &amp; PaCMAP — requires conda/miniforge (skipped gracefully if missing)</small>
        </span>
      </label>
    </div>

    <div class="install-actions">
      <button class="btn-install" onclick={handleInstall} disabled={isRunning}>
        {#if isRunning && runningCmd === "install"}
          Installing... ({elapsed})
        {:else}
          Install Packages
        {/if}
      </button>
      {#if $installState === "done"}
        <span class="status-badge done">&#10003; Done</span>
      {:else if $installState === "error"}
        <span class="status-badge error">&#10007; Error — see log below</span>
      {/if}
    </div>
  </div>

  <!-- Log output -->
  {#if $installLines.length > 0}
    <pre class="install-log" bind:this={logContainer}>{#each $installLines as line}{line}
{/each}</pre>
  {/if}

</div>

<style>
  .setup-panel {
    display: flex;
    flex-direction: column;
    height: 100%;
    padding: 1.25rem 1.5rem;
    gap: 1rem;
    overflow-y: auto;
  }

  /* Welcome */
  .welcome h2 {
    font-size: 1rem;
    font-weight: 600;
    color: #1e293b;
    margin-bottom: 0.2rem;
  }
  .welcome p {
    font-size: 0.82rem;
    color: #6b7280;
  }

  /* Check card */
  .check-card {
    border: 2px solid #2563eb;
    border-radius: 8px;
    overflow: hidden;
  }
  .check-card-label {
    background: #2563eb;
    color: #fff;
    font-size: 0.7rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    padding: 0.25rem 0.75rem;
  }
  .check-card-body {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 0.85rem 1rem;
    background: #eff6ff;
    gap: 1rem;
  }
  .check-card-text {
    display: flex;
    flex-direction: column;
    gap: 0.2rem;
  }
  .check-card-text strong {
    font-size: 0.88rem;
    color: #1e293b;
  }
  .check-card-text span {
    font-size: 0.78rem;
    color: #64748b;
  }
  .btn-check {
    padding: 0.5rem 1.2rem;
    background: #2563eb;
    color: #fff;
    border: none;
    border-radius: 5px;
    font-size: 0.84rem;
    font-weight: 600;
    font-family: inherit;
    cursor: pointer;
    white-space: nowrap;
    transition: background 0.15s;
    flex-shrink: 0;
  }
  .btn-check:hover:not(:disabled) { background: #1d4ed8; }
  .btn-check:disabled { background: #93c5fd; cursor: not-allowed; }

  /* Divider */
  .section-divider {
    display: flex;
    align-items: center;
    gap: 0.75rem;
    color: #9ca3af;
    font-size: 0.75rem;
    font-weight: 500;
  }
  .section-divider::before,
  .section-divider::after {
    content: "";
    flex: 1;
    height: 1px;
    background: #e5e7eb;
  }

  /* Install section */
  .install-section {
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }
  .install-options {
    display: flex;
    flex-direction: column;
    gap: 0.6rem;
    padding: 0.75rem 1rem;
    background: #f8fafc;
    border: 1px solid #e5e7eb;
    border-radius: 6px;
  }
  .option-row {
    display: flex;
    align-items: flex-start;
    gap: 0.6rem;
    cursor: pointer;
  }
  .option-row input[type="checkbox"] {
    margin-top: 0.15rem;
    cursor: pointer;
  }
  .option-row span {
    display: flex;
    flex-direction: column;
    gap: 0.1rem;
  }
  .option-row strong {
    font-size: 0.84rem;
    font-weight: 500;
    color: #374151;
  }
  .option-row small {
    font-size: 0.75rem;
    color: #9ca3af;
  }
  .install-actions {
    display: flex;
    align-items: center;
    gap: 0.6rem;
  }
  .btn-install {
    padding: 0.4rem 1rem;
    background: #fff;
    color: #374151;
    border: 1px solid #d1d5db;
    border-radius: 5px;
    font-size: 0.84rem;
    font-weight: 500;
    font-family: inherit;
    cursor: pointer;
    transition: background 0.15s;
  }
  .btn-install:hover:not(:disabled) { background: #f3f4f6; }
  .btn-install:disabled { color: #9ca3af; border-color: #e5e7eb; cursor: not-allowed; }

  /* Status badge */
  .status-badge {
    font-size: 0.82rem;
    font-weight: 500;
    padding: 0.25rem 0.6rem;
    border-radius: 4px;
  }
  .status-badge.done { background: #dcfce7; color: #16a34a; }
  .status-badge.error { background: #fee2e2; color: #dc2626; }

  /* Log */
  .install-log {
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
