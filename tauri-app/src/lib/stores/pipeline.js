import { writable } from "svelte/store";

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

export const installState = writable("idle"); // "idle" | "running" | "done" | "error"
export const installLines = writable([]);
export const installStartTime = writable(null);

// Optional package availability — updated by query_installed_packages on startup
// and after check_setup / install. Default true so nothing is pre-disabled.
export const packageStatus = writable({
  FastPG: true, PeacoQC: true, flowAI: true, PARC: false, pacmap: false,
});

export function addLogLine(line) {
  logLines.update((lines) => [...lines, line]);
}

export function clearLog() {
  logLines.set([]);
}
