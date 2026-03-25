import { writable } from "svelte/store";
import { listen } from "@tauri-apps/api/event";
import { invoke } from "@tauri-apps/api/core";

// Pipeline run state: "idle" | "running" | "done" | "error" | "cancelled"
export const pipelineState = writable("idle");

// Log lines
export const logLines = writable([]);

// Start time for elapsed timer
export const startTime = writable(null);

// Post-run output paths (set on successful pipeline completion)
export const pipelineOutputDir = writable(null); // Results_Files_* dir path
export const pipelineHtmlPath = writable(null);  // MARMOT_Pipeline_<name>.html path

// R environment info
export const rscriptPath = writable("");
export const rVersion = writable("");
export const marmotInstalled = writable(false);

// Quarto environment info
export const quartoPath = writable("");
export const quartoVersion = writable("");

export const installState = writable("idle"); // "idle" | "running" | "done" | "error"
export const installLines = writable([]);
export const installStartTime = writable(null);

// Optional package availability — updated by query_installed_packages on startup
// and after check_setup / install. Default true so nothing is pre-disabled.
export const packageStatus = writable({
  Rphenograph: true, PeacoQC: true, flowAI: true, PARC: false, pacmap: false,
});

export function addLogLine(line) {
  logLines.update((lines) => [...lines, line]);
}

export function clearLog() {
  logLines.set([]);
}

// ── Pipeline execution ──────────────────────────────────────────────
// Lives in the store (module scope) so event listeners survive tab switches.

let activeUnlistenLog = null;
let activeUnlistenDone = null;

function cleanupListeners() {
  if (activeUnlistenLog) { activeUnlistenLog(); activeUnlistenLog = null; }
  if (activeUnlistenDone) { activeUnlistenDone(); activeUnlistenDone = null; }
}

/**
 * Launch the pipeline. Called from Toolbar; listeners persist in module scope
 * so switching away from the setup tab does not kill them.
 */
export async function launchPipeline({ rscriptPath: rPath, metadataPath, runName, fcsFolder }) {
  // Clean up any lingering listeners from a previous run
  cleanupListeners();

  // Set up event listeners (module-scoped, not tied to any component)
  activeUnlistenLog = await listen("pipeline-log", (event) => {
    addLogLine(event.payload);
  });
  activeUnlistenDone = await listen("pipeline-done", async (event) => {
    const result = event.payload;
    pipelineState.set(result.success ? "done" : "error");
    if (result.success) {
      try {
        const resultsDir = await invoke("find_latest_results_dir", { fcsFolder });
        pipelineOutputDir.set(resultsDir);
        pipelineHtmlPath.set(`${resultsDir}/MARMOT_Pipeline_${runName}.html`);
      } catch (e) {
        console.warn("Could not locate output directory:", e);
      }
    }
    cleanupListeners();
  });

  // Start
  clearLog();
  pipelineState.set("running");
  startTime.set(Date.now());

  try {
    await invoke("run_pipeline", { rscriptPath: rPath, metadataPath, runName });
  } catch (e) {
    addLogLine("ERROR: " + e);
    pipelineState.set("error");
    cleanupListeners();
  }
}
