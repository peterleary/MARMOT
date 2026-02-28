<script>
  import { invoke } from "@tauri-apps/api/core";
  import { onMount } from "svelte";
  import Toolbar from "./lib/components/Toolbar.svelte";
  import PipelineSettings from "./lib/components/PipelineSettings.svelte";
  import StudyData from "./lib/components/StudyData.svelte";
  import FileData from "./lib/components/FileData.svelte";
  import LogPanel from "./lib/components/LogPanel.svelte";
  import ShinyPanel from "./lib/components/ShinyPanel.svelte";
  import InstallPanel from "./lib/components/InstallPanel.svelte";
  import StatusBar from "./lib/components/StatusBar.svelte";
  import SplashScreen from "./lib/components/SplashScreen.svelte";
  import { metadata } from "./lib/stores/metadata.js";
  import { rscriptPath, rVersion, marmotInstalled, packageStatus, quartoPath, quartoVersion } from "./lib/stores/pipeline.js";
  import { MARMOT_VERSION } from "./lib/stores/constants.js";

  let activeTab = $state("setup");
  let activeSetupTab = $state("install");
  let splashVisible = $state(true);
  let splashFading = $state(false);
  let splashStatus = $state("Starting up...");
  let splashMissing = $state([]);

  function hideSplash() {
    splashFading = true;
    setTimeout(() => { splashVisible = false; }, 400);
  }

  const tabs = [
    { id: "setup",  label: "Setup",   icon: "&#128230;" },
    { id: "log",    label: "Run/log", icon: "&#9654;" },
    { id: "shiny",  label: "Shiny",   icon: "&#128202;" },
  ];

  const setupTabs = [
    { id: "install", label: "Install",      icon: "&#128230;" },
    { id: "settings", label: "Settings",    icon: "&#9881;" },
    { id: "study",   label: "Study Design", icon: "&#9878;" },
    { id: "files",   label: "Files",        icon: "&#128194;" },
  ];

  function setActiveTab(tab) {
    activeTab = tab;
  }

  onMount(async () => {
    splashStatus = "Loading metadata and detecting R & Quarto...";

    // Metadata load + Rscript + Quarto detection in parallel (independent operations)
    const [rPath, , qPath] = await Promise.all([
      invoke("find_rscript_cached").catch(() => null),
      invoke("load_default_metadata")
        .then((d) => metadata.set(d))
        .catch(() =>
          invoke("create_new_metadata")
            .then((d) => metadata.set(d))
            .catch(() => {})
        ),
      invoke("find_quarto_cached").catch(() => null),
    ]);

    // Quarto detection
    if (qPath) {
      quartoPath.set(qPath);
      invoke("get_quarto_info", { quartoPath: qPath })
        .then((ver) => quartoVersion.set(ver))
        .catch(() => {});
    }

    // Block startup if R or Quarto is missing
    const deps = [];
    if (!rPath) {
      deps.push({ name: "R", url: "https://cloud.r-project.org/", description: "Statistical computing environment" });
    }
    if (!qPath) {
      deps.push({ name: "Quarto", url: "https://quarto.org/docs/get-started/", description: "Document rendering engine" });
    }
    if (deps.length > 0) {
      splashMissing = deps;
      return; // stay on splash screen
    }

    rscriptPath.set(rPath);

    splashStatus = "Checking R environment...";

    // One R subprocess for version + MARMOT check (saves ~500ms vs two sequential calls)
    try {
      const [version, installed] = await invoke("get_r_info", { rscriptPath: rPath });
      rVersion.set(version);
      marmotInstalled.set(installed);
    } catch (e) {
      console.warn("R info check failed:", e);
    }

    hideSplash();

    // Optional packages: background, non-blocking
    invoke("query_installed_packages", { rscriptPath: rPath })
      .then((status) => packageStatus.set(status))
      .catch(() => {});
  });
</script>

{#if splashVisible}
  <div class="splash-wrapper" class:fading={splashFading}>
    <SplashScreen status={splashStatus} missing={splashMissing} />
  </div>
{/if}

<div class="app">
  <header class="app-header">
    <div class="logo-area">
      <img src="/marmot-logo.png" alt="MARMOT" class="logo-img" />
      <div class="logo-text-area">
        <span class="logo-text">MARMOT</span>
        <span class="logo-subtitle">Flow Cytometry Analysis</span>
      </div>
    </div>
    <div class="header-right">
      <span class="version-tag">v{MARMOT_VERSION}</span>
    </div>
  </header>

  <Toolbar onActiveTab={setActiveTab} />

  <div class="tab-bar">
    {#each tabs as tab}
      <button
        class="tab-btn"
        class:active={activeTab === tab.id}
        onclick={() => setActiveTab(tab.id)}
      >
        <span class="tab-icon">{@html tab.icon}</span>
        {tab.label}
      </button>
    {/each}
  </div>

  <main class="tab-content">
    {#if activeTab === "setup"}
      <div class="setup-layout">
        <nav class="setup-subnav">
          {#each setupTabs as st}
            <button
              class="subnav-btn"
              class:active={activeSetupTab === st.id}
              onclick={() => (activeSetupTab = st.id)}
            >
              <span class="subnav-icon">{@html st.icon}</span>
              {st.label}
            </button>
          {/each}
        </nav>
        <div class="setup-content">
          {#if activeSetupTab === "install"}
            <InstallPanel />
          {:else if activeSetupTab === "settings"}
            <PipelineSettings />
          {:else if activeSetupTab === "study"}
            <StudyData />
          {:else if activeSetupTab === "files"}
            <FileData />
          {/if}
        </div>
      </div>
    {:else if activeTab === "log"}
      <LogPanel />
    {:else if activeTab === "shiny"}
      <ShinyPanel />
    {/if}
  </main>

  <StatusBar />
</div>

<style>
  .splash-wrapper {
    opacity: 1;
    transition: opacity 0.4s ease;
  }
  .splash-wrapper.fading {
    opacity: 0;
    pointer-events: none;
  }

  :global(*) {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
  }
  :global(body) {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
      "Helvetica Neue", Arial, sans-serif;
    background: #f8fafc;
    color: #333;
    overflow: hidden;
  }
  .app {
    display: flex;
    flex-direction: column;
    height: 100vh;
  }
  .app-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 0.55rem 0.9rem;
    background: linear-gradient(135deg, #1e293b 0%, #334155 100%);
    color: #fff;
    user-select: none;
  }
  .logo-area {
    display: flex;
    align-items: center;
    gap: 0.6rem;
  }
  .logo-img {
    width: 32px;
    height: 32px;
    border-radius: 5px;
  }
  .logo-text-area {
    display: flex;
    flex-direction: column;
    gap: 0;
  }
  .logo-text {
    font-size: 1.15rem;
    font-weight: 700;
    letter-spacing: 0.08em;
    color: #dbeafe;
    line-height: 1.1;
  }
  .logo-subtitle {
    font-size: 0.7rem;
    color: #94a3b8;
    font-weight: 400;
    letter-spacing: 0.02em;
  }
  .header-right {
    display: flex;
    align-items: center;
  }
  .version-tag {
    font-size: 0.68rem;
    color: #64748b;
    background: rgba(255,255,255,0.08);
    padding: 0.15rem 0.45rem;
    border-radius: 3px;
    font-family: "SF Mono", "Fira Code", monospace;
  }
  .tab-bar {
    display: flex;
    background: #f0f5ff;
    border-bottom: 2px solid #dbeafe;
    padding: 0 0.75rem;
    gap: 0.15rem;
  }
  .tab-btn {
    padding: 0.55rem 1rem;
    border: none;
    background: transparent;
    cursor: pointer;
    font-size: 0.84rem;
    color: #888;
    font-weight: 500;
    border-bottom: 2.5px solid transparent;
    margin-bottom: -2px;
    font-family: inherit;
    transition: color 0.15s, border-color 0.15s, background 0.15s;
    border-radius: 5px 5px 0 0;
    display: flex;
    align-items: center;
    gap: 0.35rem;
  }
  .tab-btn:hover {
    color: #555;
    background: rgba(37, 99, 235, 0.05);
  }
  .tab-btn.active {
    color: #2563eb;
    border-bottom-color: #2563eb;
    background: #fff;
  }
  .tab-icon {
    font-size: 0.82rem;
    opacity: 0.7;
  }
  .tab-btn.active .tab-icon {
    opacity: 1;
  }
  .tab-content {
    flex: 1;
    overflow: hidden;
    background: #fff;
    display: flex;
    flex-direction: column;
  }

  /* Setup: sidebar sub-nav + content */
  .setup-layout {
    display: flex;
    flex: 1;
    overflow: hidden;
  }
  .setup-subnav {
    display: flex;
    flex-direction: column;
    width: 148px;
    min-width: 148px;
    background: #f8fafc;
    border-right: 1px solid #e2e8f0;
    padding: 0.5rem 0.4rem;
    gap: 0.1rem;
  }
  .subnav-btn {
    display: flex;
    align-items: center;
    gap: 0.45rem;
    padding: 0.5rem 0.65rem;
    border: none;
    border-radius: 6px;
    background: transparent;
    cursor: pointer;
    font-size: 0.82rem;
    font-family: inherit;
    color: #64748b;
    font-weight: 500;
    text-align: left;
    transition: background 0.12s, color 0.12s;
    white-space: nowrap;
  }
  .subnav-btn:hover {
    background: #e2e8f0;
    color: #334155;
  }
  .subnav-btn.active {
    background: #dbeafe;
    color: #1d4ed8;
    font-weight: 600;
  }
  .subnav-icon {
    font-size: 0.8rem;
    opacity: 0.75;
  }
  .subnav-btn.active .subnav-icon {
    opacity: 1;
  }
  .setup-content {
    flex: 1;
    overflow-y: auto;
  }
</style>
