<script>
  import { invoke } from "@tauri-apps/api/core";
  import { listen } from "@tauri-apps/api/event";
  import { rscriptPath, installState, installLines, installStartTime, packageStatus } from "../stores/pipeline.js";

  let logContainer = $state(null);
  let includeSuggests = $state(true);
  let includePython = $state(true);
  let elapsed = $state("0:00");
  let timer = $state(null);
  let runningCmd = $state(null); // "check" | "marmot" | "extras" | null

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

  async function handleInstallMarmot() {
    await runCommand("marmot", () =>
      invoke("run_install_marmot", { rscriptPath: $rscriptPath })
    );
  }

  async function handleInstallExtras() {
    await runCommand("extras", () =>
      invoke("run_install_extras", {
        rscriptPath: $rscriptPath,
        includeSuggests,
        includePython,
      })
    );
  }

  let isRunning = $derived($installState === "running");
</script>

<div class="setup-panel">

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
    <span>Install</span>
  </div>

  <!-- Step 1: Install MARMOT -->
  <div class="install-card">
    <div class="install-card-header">
      <span class="install-step">1</span>
      <div class="install-card-text">
        <strong>Install MARMOT</strong>
        <span>Installs the MARMOT R package and all core dependencies from GitHub. This is required before anything else.</span>
      </div>
    </div>
    <div class="install-card-action">
      <button class="btn-primary" onclick={handleInstallMarmot} disabled={isRunning}>
        {#if isRunning && runningCmd === "marmot"}
          Installing MARMOT... ({elapsed})
        {:else}
          Install MARMOT
        {/if}
      </button>
    </div>
  </div>

  <!-- Step 2: Install Extras -->
  <div class="install-card">
    <div class="install-card-header">
      <span class="install-step">2</span>
      <div class="install-card-text">
        <strong>Install extras</strong>
        <span>Optional packages and the Python environment for PARC/PaCMAP. Not everything may install on every system &mdash; that's fine, MARMOT works without them.</span>
      </div>
    </div>
    <div class="install-card-options">
      <label class="option-row">
        <input type="checkbox" bind:checked={includeSuggests} disabled={isRunning} />
        <span>
          <strong>Optional R packages</strong>
          <small>Seurat, Rphenograph, etc.</small>
        </span>
      </label>
      <label class="option-row">
        <input type="checkbox" bind:checked={includePython} disabled={isRunning} />
        <span>
          <strong>Python environment</strong>
          <small>PARC &amp; PaCMAP &mdash; automatic via basilisk, no conda needed</small>
        </span>
      </label>
    </div>
    <div class="install-card-action">
      <button class="btn-secondary" onclick={handleInstallExtras} disabled={isRunning}>
        {#if isRunning && runningCmd === "extras"}
          Installing extras... ({elapsed})
        {:else}
          Install Extras
        {/if}
      </button>
    </div>
  </div>

  <!-- Status badge -->
  {#if $installState === "done" || $installState === "error"}
    <div class="status-row">
      {#if $installState === "done"}
        <span class="status-badge done">&#10003; Done</span>
      {:else}
        <span class="status-badge error">&#10007; Error &mdash; see log below</span>
      {/if}
    </div>
  {/if}

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

  /* Install cards */
  .install-card {
    border: 1px solid #e2e8f0;
    border-radius: 8px;
    overflow: hidden;
  }
  .install-card-header {
    display: flex;
    align-items: flex-start;
    gap: 0.75rem;
    padding: 0.85rem 1rem 0.5rem;
  }
  .install-step {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 24px;
    height: 24px;
    min-width: 24px;
    background: #334155;
    color: #fff;
    font-size: 0.75rem;
    font-weight: 700;
    border-radius: 50%;
    margin-top: 0.05rem;
  }
  .install-card-text {
    display: flex;
    flex-direction: column;
    gap: 0.15rem;
  }
  .install-card-text strong {
    font-size: 0.88rem;
    color: #1e293b;
  }
  .install-card-text span {
    font-size: 0.78rem;
    color: #64748b;
    line-height: 1.45;
  }
  .install-card-options {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
    padding: 0.5rem 1rem 0.5rem 3rem;
  }
  .install-card-action {
    padding: 0.5rem 1rem 0.85rem 3rem;
  }

  /* Buttons */
  .btn-primary {
    padding: 0.5rem 1.4rem;
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
  }
  .btn-primary:hover:not(:disabled) { background: #1d4ed8; }
  .btn-primary:disabled { background: #93c5fd; cursor: not-allowed; }

  .btn-secondary {
    padding: 0.45rem 1.2rem;
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
  .btn-secondary:hover:not(:disabled) { background: #f3f4f6; }
  .btn-secondary:disabled { color: #9ca3af; border-color: #e5e7eb; cursor: not-allowed; }

  /* Option rows */
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

  /* Status */
  .status-row {
    display: flex;
    align-items: center;
  }
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
