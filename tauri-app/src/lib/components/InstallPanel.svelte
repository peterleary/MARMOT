<script>
  import { invoke } from "@tauri-apps/api/core";
  import { listen } from "@tauri-apps/api/event";
  import { rscriptPath, installState, installLines, installStartTime, packageStatus, marmotInstalled } from "../stores/pipeline.js";

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
      try {
        // Single R boot for MARMOT presence + optional package status.
        const [, installed, status] = await invoke("get_r_status", { rscriptPath: $rscriptPath });
        if (e.payload.success) marmotInstalled.set(installed);
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
  let prereqOpen = $state(false);

  const ua = navigator.userAgent || "";
  const isMac = ua.includes("Mac");
  const isWindows = ua.includes("Win");
  const isLinux = !isMac && !isWindows;
</script>

<div class="panel">

  <div class="panel-content">

  <!-- Check Setup -->
  <div class="check-row">
    <div>
      <div class="section-title">Check your setup</div>
      <p class="hint">Scans your R environment and reports which packages are installed.</p>
    </div>
    <button class="btn blue" onclick={handleCheckSetup} disabled={isRunning}>
      {#if isRunning && runningCmd === "check"}Checking... ({elapsed}){:else}Check Setup{/if}
    </button>
  </div>

  <hr />

  <!-- Step 1 -->
  <div class="section-header">
    <span class="step-num">1</span>
    <div>
      <div class="section-title">Install MARMOT</div>
      <p class="hint">Core package. No compiler needed on macOS or Windows &mdash; all dependencies install as pre-built binaries.</p>
    </div>
  </div>

  <div class="two-col">
    <div class="col col-left">
      <div class="col-title">What gets installed</div>
      <div class="row"><strong>MARMOT R package</strong> <span class="detail">from GitHub</span></div>
      <div class="row"><strong>Bioconductor</strong> <span class="detail">FlowSOM, CATALYST, diffcyt, flowCore, ...</span></div>
      <div class="row"><strong>CRAN</strong> <span class="detail">ggplot2, arrow, igraph, shiny, ...</span></div>
    </div>
    <div class="col col-right">
      <div class="col-title">What you can use</div>
      <div class="row ok"><span class="icon green">&#10003;</span> <strong>Clustering</strong> <span class="detail">FlowSOM, Mphenograph, MfastPG, Mparc</span></div>
      <div class="row ok"><span class="icon green">&#10003;</span> <strong>Dimensionality reduction</strong> <span class="detail">UMAP, tSNE, Mpacmap</span></div>
      <div class="row ok"><span class="icon green">&#10003;</span> <strong>Full pipeline + Shiny app</strong> <span class="detail">DA/DS analysis, heatmaps, exports</span></div>
    </div>
  </div>

  <p class="note green-note"><strong>Prerequisites:</strong> R 4.5+ only.</p>
  <p class="note amber-note">
    Mparc, Mpacmap, and Mphenograph are convenience R reimplementations bundled with MARMOT.
    They are provided as fallbacks only &mdash; results may differ from the original implementations.
    For production use, install the extras below.
  </p>

  <button class="btn blue" onclick={handleInstallMarmot} disabled={isRunning}>
    {#if isRunning && runningCmd === "marmot"}Installing MARMOT... ({elapsed}){:else}Install MARMOT{/if}
  </button>

  <hr />

  <!-- Step 2 -->
  <div class="section-header">
    <span class="step-num amber">2</span>
    <div>
      <div class="section-title">Install extras &mdash; recommended</div>
      <p class="hint">
        The original C++ and Python implementations of PARC, PaCMAP, and Rphenograph.
        These are the <em>published reference implementations</em>, significantly faster on large datasets,
        and the code against which MARMOT was validated.
        We take no responsibility for differences in results between the R fallbacks and the originals.
      </p>
    </div>
  </div>

  <div class="two-col">
    <div class="col col-left">
      <div class="col-title">What gets installed</div>
      <label class="row clickable">
        <input type="checkbox" bind:checked={includeSuggests} disabled={isRunning} />
        <span><strong>C++ packages</strong> <span class="detail">Rphenograph, Seurat, etc.</span></span>
      </label>
      <label class="row clickable">
        <input type="checkbox" bind:checked={includePython} disabled={isRunning} />
        <span><strong>Python environment</strong> <span class="detail">PARC + PaCMAP via conda (p4r env)</span></span>
      </label>
    </div>
    <div class="col col-right">
      <div class="col-title">What you gain</div>
      <div class="row up"><span class="icon amber">&#9650;</span> <strong>Rphenograph</strong> <span class="detail">Original C++ Louvain (validated)</span></div>
      <div class="row up"><span class="icon amber">&#9650;</span> <strong>Python PARC</strong> <span class="detail">Reference implementation (published)</span></div>
      <div class="row up"><span class="icon amber">&#9650;</span> <strong>Python PaCMAP</strong> <span class="detail">Reference implementation (published)</span></div>
    </div>
  </div>

  <button class="prereq-toggle" onclick={() => prereqOpen = !prereqOpen}>
    <span>{prereqOpen ? "\u25BE" : "\u25B8"}</span>
    Prerequisites for extras
  </button>
  {#if prereqOpen}
    <div class="prereq-body">
      <p><strong>C++ packages</strong> require a compiler:</p>
      {#if isMac}
        <p>Install <a href="https://brew.sh" target="_blank" rel="noopener">Homebrew</a> (also installs Xcode CLT). In Terminal:</p>
        <pre class="cmd">/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"</pre>
      {:else if isWindows}
        <p>Install <a href="https://cran.r-project.org/bin/windows/Rtools/" target="_blank" rel="noopener">Rtools</a> matching your R version (e.g. Rtools45 for R 4.5.x).</p>
      {:else}
        <pre class="cmd">sudo apt install build-essential libcurl4-openssl-dev libssl-dev libxml2-dev</pre>
      {/if}
      <p style="margin-top:0.5rem"><strong>Python environment</strong> requires conda/mamba:</p>
      <p>Install <a href="https://github.com/conda-forge/miniforge#miniforge3" target="_blank" rel="noopener">Miniforge</a> (lightweight conda with mamba).</p>
    </div>
  {/if}

  <button class="btn outline" onclick={handleInstallExtras} disabled={isRunning}>
    {#if isRunning && runningCmd === "extras"}Installing extras... ({elapsed}){:else}Install Extras{/if}
  </button>

  </div>

  <!-- Status + Log (always visible at bottom) -->
  {#if $installState === "done" || $installState === "error" || $installLines.length > 0}
    <div class="log-area">
      {#if $installState === "done" || $installState === "error"}
        <div class="status" class:done={$installState === "done"} class:error={$installState === "error"}>
          {#if $installState === "done"}&#10003; Done{:else}&#10007; Error{/if}
        </div>
      {/if}
      <pre class="log" bind:this={logContainer}>{#each $installLines as line}{line}
{/each}</pre>
    </div>
  {/if}

</div>

<style>
  .panel {
    height: 100%;
    overflow: hidden;
    display: flex;
    flex-direction: column;
  }
  .panel-content {
    flex: 1 1 auto;
    overflow-y: auto;
    padding: 1.25rem 1.5rem;
    display: flex;
    flex-direction: column;
    gap: 0.6rem;
  }
  .log-area {
    flex: 0 0 auto;
    max-height: 40%;
    display: flex;
    flex-direction: column;
    padding: 0 1.5rem 1rem;
    gap: 0.4rem;
  }

  hr {
    border: none;
    border-top: 1px solid #e5e7eb;
    margin: 0.25rem 0;
  }

  /* Section headers */
  .section-header {
    display: flex;
    align-items: flex-start;
    gap: 0.6rem;
  }
  .section-title {
    font-size: 0.88rem;
    font-weight: 600;
    color: #1e293b;
  }
  .hint {
    font-size: 0.78rem;
    color: #64748b;
    line-height: 1.45;
    margin: 0.15rem 0 0;
  }
  .hint em { font-weight: 600; }
  .step-num {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 22px; height: 22px; min-width: 22px;
    background: #334155;
    color: #fff;
    font-size: 0.72rem;
    font-weight: 700;
    border-radius: 50%;
    margin-top: 0.1rem;
  }
  .step-num.amber { background: #f59e0b; }

  /* Check row */
  .check-row {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 1rem;
  }

  /* Two-column grid */
  .two-col {
    display: grid;
    grid-template-columns: 1fr 1fr;
    border: 1px solid #e2e8f0;
    border-radius: 6px;
  }
  .col { padding: 0.5rem 0.7rem; }
  .col-left {
    border-right: 1px solid #e2e8f0;
    background: #f8fafc;
  }
  .col-right { background: #fff; }
  .col-title {
    font-size: 0.68rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: #94a3b8;
    margin-bottom: 0.35rem;
  }
  .row {
    display: flex;
    flex-wrap: wrap;
    align-items: baseline;
    gap: 0 0.35rem;
    padding: 0.2rem 0;
    font-size: 0.78rem;
    color: #334155;
  }
  .row + .row { border-top: 1px solid #f1f5f9; }
  .row strong { font-weight: 500; }
  .detail {
    font-size: 0.7rem;
    color: #94a3b8;
    width: 100%;
  }
  .icon { font-size: 0.68rem; flex-shrink: 0; }
  .icon.green { color: #16a34a; }
  .icon.amber { color: #f59e0b; font-size: 0.58rem; }

  /* Checkbox rows */
  .row.clickable {
    cursor: pointer;
    gap: 0.4rem;
  }
  .row.clickable input[type="checkbox"] {
    margin: 0;
    cursor: pointer;
    flex-shrink: 0;
  }
  .row.clickable span {
    display: flex;
    flex-direction: column;
  }

  /* Notes */
  .note {
    font-size: 0.74rem;
    line-height: 1.45;
    padding: 0.35rem 0.6rem;
    border-radius: 5px;
    margin: 0;
  }
  .green-note {
    background: #f0fdf4;
    border: 1px solid #bbf7d0;
    color: #15803d;
  }
  .amber-note {
    background: #fefce8;
    border: 1px solid #fde68a;
    color: #854d0e;
  }

  /* Prereq toggle + body */
  .prereq-toggle {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    background: none;
    border: 1px solid #e2e8f0;
    border-radius: 5px;
    padding: 0.4rem 0.65rem;
    font-size: 0.76rem;
    font-weight: 600;
    color: #92400e;
    cursor: pointer;
    font-family: inherit;
    text-align: left;
    width: fit-content;
  }
  .prereq-toggle:hover { background: #fffbeb; }
  .prereq-body {
    font-size: 0.76rem;
    color: #475569;
    line-height: 1.5;
    padding: 0 0.2rem;
  }
  .prereq-body p { margin: 0.2rem 0; }
  .prereq-body a { color: #2563eb; text-decoration: underline; }
  .cmd {
    background: #f1f5f9;
    padding: 0.3rem 0.5rem;
    border-radius: 4px;
    font-family: "SF Mono", "Fira Code", "Consolas", monospace;
    font-size: 0.72rem;
    white-space: pre-wrap;
    word-break: break-word;
    margin: 0.2rem 0;
  }

  /* Buttons */
  .btn {
    padding: 0.45rem 1.2rem;
    border: none;
    border-radius: 5px;
    font-size: 0.82rem;
    font-weight: 600;
    font-family: inherit;
    cursor: pointer;
    transition: background 0.15s;
    width: fit-content;
    white-space: nowrap;
    flex-shrink: 0;
  }
  .btn.blue { background: #2563eb; color: #fff; }
  .btn.blue:hover:not(:disabled) { background: #1d4ed8; }
  .btn.blue:disabled { background: #93c5fd; cursor: not-allowed; }
  .btn.outline {
    background: #fff;
    color: #374151;
    border: 1px solid #d1d5db;
    font-weight: 500;
  }
  .btn.outline:hover:not(:disabled) { background: #f3f4f6; }
  .btn.outline:disabled { color: #9ca3af; border-color: #e5e7eb; cursor: not-allowed; }

  /* Status */
  .status {
    font-size: 0.82rem;
    font-weight: 500;
    padding: 0.25rem 0.6rem;
    border-radius: 4px;
    width: fit-content;
  }
  .status.done { background: #dcfce7; color: #16a34a; }
  .status.error { background: #fee2e2; color: #dc2626; }

  /* Log */
  .log {
    background: #1e1e1e;
    color: #d4d4d4;
    padding: 0.75rem;
    border-radius: 6px;
    font-family: "SF Mono", "Fira Code", "Consolas", monospace;
    font-size: 0.78rem;
    line-height: 1.5;
    overflow-y: auto;
    margin: 0;
    flex: 1 1 0;
    min-height: 100px;
    white-space: pre-wrap;
    word-break: break-word;
  }
</style>
