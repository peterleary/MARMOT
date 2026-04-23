import { writable, derived } from "svelte/store";

// The full metadata object matching Rust MetadataFile
export const metadata = writable({
  path: null,
  pipeline_settings: [],
  study_data: { headers: [], rows: [] },
  file_data: { headers: [], rows: [] },
});

// FCS folder path
export const fcsFolder = writable("");

// Run name
export const runName = writable("My MARMOT Analysis");

// Dirty flag - metadata has unsaved changes
export const isDirty = writable(false);

// Last pre-flight report (null until user runs it). Cleared whenever metadata
// changes so a stale green report can't mislead the user into running a broken
// config.
export const preflightReport = writable(null);
export const preflightChecking = writable(false);

let skipFirstMetadata = true;
let skipFirstFolder = true;
metadata.subscribe(() => {
  if (skipFirstMetadata) { skipFirstMetadata = false; return; }
  preflightReport.set(null);
});
fcsFolder.subscribe(() => {
  if (skipFirstFolder) { skipFirstFolder = false; return; }
  preflightReport.set(null);
});

// Helper to get a setting value
export function getSettingValue(settings, variable) {
  const s = settings.find((s) => s.variable === variable);
  return s ? s.setting : "";
}

// Helper to update a setting value
export function updateSetting(variable, value) {
  metadata.update((m) => {
    const idx = m.pipeline_settings.findIndex((s) => s.variable === variable);
    if (idx >= 0) {
      m.pipeline_settings[idx] = { ...m.pipeline_settings[idx], setting: String(value) };
    }
    return { ...m, pipeline_settings: [...m.pipeline_settings] };
  });
  isDirty.set(true);
}
