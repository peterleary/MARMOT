import { writable, derived } from "svelte/store";

// The full metadata object matching Rust MetadataFile
export const metadata = writable({
  path: null,
  pipeline_settings: [],
  study_data: { headers: [], rows: [] },
  file_data: { headers: [], rows: [] },
  options: { headers: [], rows: [] },
});

// FCS folder path
export const fcsFolder = writable("");

// Run name
export const runName = writable("My MARMOT Analysis");

// Dirty flag - metadata has unsaved changes
export const isDirty = writable(false);

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
      m.pipeline_settings[idx].setting = String(value);
    }
    return m;
  });
  isDirty.set(true);
}
