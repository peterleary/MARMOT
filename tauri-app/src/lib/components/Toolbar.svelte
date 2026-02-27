<script>
  import { invoke } from "@tauri-apps/api/core";
  import { open, save } from "@tauri-apps/plugin-dialog";
  import { metadata, isDirty, fcsFolder, runName } from "../stores/metadata.js";
  import { pipelineState, logLines, startTime, clearLog, addLogLine, rscriptPath,
           pipelineOutputDir, pipelineHtmlPath, quartoPath } from "../stores/pipeline.js";
  import { validateSettings, validateFileData } from "../utils/validation.js";
  import { listen } from "@tauri-apps/api/event";

  let { onActiveTab = () => {} } = $props();

  async function handleOpen() {
    const selected = await open({
      title: "Open MARMOT Metadata",
      filters: [{ name: "Excel", extensions: ["xlsx"] }],
    });
    if (!selected) return;

    try {
      const data = await invoke("read_excel", { path: selected });
      metadata.set(data);
      isDirty.set(false);

      // Auto-fill FCS folder with the Excel file's parent directory
      const sep = selected.includes("/") ? "/" : "\\";
      const parent = selected.substring(0, selected.lastIndexOf(sep));
      if (parent) fcsFolder.set(parent);
    } catch (e) {
      alert("Failed to open file: " + e);
    }
  }

  async function handleSave() {
    let path = $metadata.path;
    if (!path) {
      path = await save({
        title: "Save MARMOT Metadata",
        filters: [{ name: "Excel", extensions: ["xlsx"] }],
        defaultPath: "MARMOT_Metadata.xlsx",
      });
      if (!path) return;
    }

    try {
      await invoke("write_excel", { metadata: $metadata, path });
      metadata.update((m) => ({ ...m, path }));
      isDirty.set(false);
    } catch (e) {
      alert("Failed to save file: " + e);
    }
  }

  async function handleNew() {
    if ($isDirty && !confirm("You have unsaved changes. Create new metadata?")) return;
    try {
      const data = await invoke("create_new_metadata");
      metadata.set(data);
      isDirty.set(false);
    } catch (e) {
      alert("Failed to create new metadata: " + e);
    }
  }

  async function handleBrowse() {
    const selected = await open({ directory: true, title: "Select FCS folder" });
    if (selected) {
      fcsFolder.set(selected);
    }
  }

  async function handleRun() {
    // Validate
    const settingsErrors = validateSettings($metadata.pipeline_settings);
    const fileErrors = validateFileData($metadata.file_data);
    const allErrors = [...settingsErrors, ...fileErrors];

    if (allErrors.length > 0) {
      alert("Validation errors:\n\n" + allErrors.join("\n"));
      return;
    }

    if (!$fcsFolder) {
      alert("Please select an FCS folder first.");
      return;
    }

    if (!$rscriptPath) {
      alert("Rscript not found. Please check R installation.");
      return;
    }

    if (!$quartoPath) {
      alert("Quarto not found. Install it from https://quarto.org/docs/get-started/");
      return;
    }

    // Capture folder and runName before async operations (stores may change)
    const folder = $fcsFolder;
    const name = $runName;

    // Save metadata to temp location
    const metadataPath = folder + "/" + name.replace(/[^A-Za-z0-9]/g, "_") + "_metadata.xlsx";
    try {
      await invoke("write_excel", { metadata: $metadata, path: metadataPath });
    } catch (e) {
      alert("Failed to save metadata: " + e);
      return;
    }

    // Clear stale post-run state before starting a new run
    pipelineOutputDir.set(null);
    pipelineHtmlPath.set(null);

    // Set up event listeners
    const unlistenLog = await listen("pipeline-log", (event) => {
      addLogLine(event.payload);
    });
    const unlistenDone = await listen("pipeline-done", async (event) => {
      const result = event.payload;
      pipelineState.set(result.success ? "done" : "error");
      if (result.success) {
        try {
          const resultsDir = await invoke("find_latest_results_dir", { fcsFolder: folder });
          pipelineOutputDir.set(resultsDir);
          pipelineHtmlPath.set(`${folder}/MARMOT_Pipeline_${name}.html`);
        } catch (e) {
          console.warn("Could not locate output directory:", e);
        }
      }
      unlistenLog();
      unlistenDone();
    });

    // Start pipeline
    clearLog();
    pipelineState.set("running");
    startTime.set(Date.now());
    onActiveTab("log");

    try {
      await invoke("run_pipeline", {
        rscriptPath: $rscriptPath,
        metadataPath,
        runName: name,
      });
    } catch (e) {
      addLogLine("ERROR: " + e);
      pipelineState.set("error");
      unlistenLog();
      unlistenDone();
    }
  }
</script>

<div class="toolbar">
  <div class="toolbar-left">
    <span class="toolbar-group-label">Metadata</span>
    <button class="btn" onclick={handleNew} title="Create new metadata">New</button>
    <button class="btn" onclick={handleOpen} title="Open metadata Excel">Open</button>
    <button class="btn" onclick={handleSave} title="Save metadata Excel">Save{$isDirty ? " *" : ""}</button>

    <div class="separator"></div>

    <div class="folder-input">
      <label for="fcs-folder">FCS Folder:</label>
      <input id="fcs-folder" type="text" bind:value={$fcsFolder} placeholder="Select folder..." readonly />
      <button class="btn btn-sm" onclick={handleBrowse}>Browse</button>
    </div>
  </div>

  <div class="toolbar-right">
    <div class="run-name-input">
      <label for="run-name">Run Name:</label>
      <input id="run-name" type="text" bind:value={$runName} />
    </div>
    <button
      class="btn btn-run"
      onclick={handleRun}
      disabled={$pipelineState === "running"}
    >
      {$pipelineState === "running" ? "Running..." : "Run Pipeline"}
    </button>
  </div>
</div>

<style>
  .toolbar {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem 0.75rem;
    background: #f0f5ff;
    border-bottom: 1px solid #dbeafe;
    gap: 1rem;
    flex-wrap: wrap;
  }
  .toolbar-left, .toolbar-right {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }
  .toolbar-group-label {
    font-size: 0.72rem;
    font-weight: 600;
    color: #94a3b8;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    margin-right: 0.1rem;
    user-select: none;
  }
  .btn {
    padding: 0.35rem 0.7rem;
    border: 1px solid #bfdbfe;
    border-radius: 4px;
    background: #fff;
    cursor: pointer;
    font-size: 0.82rem;
    color: #333;
    font-family: inherit;
  }
  .btn:hover:not(:disabled) {
    background: #dbeafe;
    border-color: #2563eb;
  }
  .btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
  .btn-sm {
    padding: 0.25rem 0.5rem;
    font-size: 0.78rem;
  }
  .btn-run {
    background: #2563eb;
    color: #fff;
    border-color: #1d4ed8;
    font-weight: 600;
    padding: 0.4rem 1rem;
  }
  .btn-run:hover:not(:disabled) {
    background: #1d4ed8;
  }
  .separator {
    width: 1px;
    height: 24px;
    background: #ccc;
    margin: 0 0.3rem;
  }
  .folder-input, .run-name-input {
    display: flex;
    align-items: center;
    gap: 0.3rem;
  }
  .folder-input label, .run-name-input label {
    font-size: 0.8rem;
    color: #555;
    white-space: nowrap;
  }
  .folder-input input, .run-name-input input {
    padding: 0.3rem 0.5rem;
    border: 1px solid #ccc;
    border-radius: 4px;
    font-size: 0.8rem;
    font-family: inherit;
  }
  .folder-input input {
    width: 200px;
    background: #fafafa;
  }
  .run-name-input input {
    width: 160px;
  }
</style>
