import { invoke } from "@tauri-apps/api/core";
import { get } from "svelte/store";
import { metadata, isDirty, fcsFolder } from "../stores/metadata.js";

/**
 * Point the app at a folder of FCS files. Enumerates the `.fcs` files,
 * populates the file_data table (one row per file, file_name + sample_id
 * pre-filled), peeks the first file for marker info, and auto-fills the
 * Study Data markers column if it's currently empty.
 *
 * Returns a small status object used by the UI to show info/warning banners.
 *   { kind: "info" | "warn", text: string } | null
 *
 * Shared between Toolbar's Browse button and FileData's Scan FCS Folder
 * button so both entry points behave identically.
 */
export async function scanFcsFolder(selectedPath) {
  fcsFolder.set(selectedPath);

  const files = await invoke("scan_fcs_folder", { path: selectedPath });

  metadata.update((m) => {
    if (!m.file_data.headers.includes("file_name")) {
      m.file_data.headers = [
        "file_name",
        "sample_id",
        "condition",
        ...m.file_data.headers.filter(
          (h) => !["file_name", "sample_id", "condition"].includes(h)
        ),
      ];
    }

    const fnIdx = m.file_data.headers.indexOf("file_name");
    const sidIdx = m.file_data.headers.indexOf("sample_id");

    m.file_data.rows = files.map((fname) => {
      const row = m.file_data.headers.map(() => "");
      row[fnIdx] = fname;
      if (sidIdx >= 0) {
        row[sidIdx] = fname.replace(/\.fcs$/i, "");
      }
      return row;
    });
    return m;
  });

  isDirty.set(true);

  if (files.length === 0) {
    return { kind: "warn", text: `No .fcs files found in ${selectedPath}.` };
  }

  // Peek the first FCS file for marker info
  try {
    const firstPath = `${selectedPath}/${files[0]}`;
    const peeked = await invoke("peek_fcs_markers", { path: firstPath });
    return autoFillMarkers(peeked, files[0]);
  } catch (e) {
    console.warn("Marker peek failed:", e);
    return {
      kind: "warn",
      text: `Could not read markers from ${files[0]}: ${e}`,
    };
  }
}

// Prefer $PnS (long name) when present; fall back to $PnN (channel name).
// Each channel appears in EXACTLY ONE column:
//   - real markers → "Markers to include"
//   - scatter / time / CyTOF metadata → "Markers to exclude completely"
// The user can move individual entries between columns if they disagree
// (e.g. keep SSC-A as a lineage feature by cutting it from exclude and
// pasting into include).
function autoFillMarkers(peeked, sourceFile) {
  const asName = (p) =>
    (p.long_name && p.long_name.trim()) || p.short_name.trim();

  const includeNames = peeked
    .filter((p) => !p.is_scatter)
    .map(asName)
    .filter((n) => n);
  const excludeNames = peeked
    .filter((p) => p.is_scatter)
    .map(asName)
    .filter((n) => n);

  if (includeNames.length === 0 && excludeNames.length === 0) {
    return { kind: "warn", text: `No channels detected in ${sourceFile}.` };
  }

  let filled = false;
  let existed = false;
  metadata.update((m) => {
    const mkIdx = m.study_data.headers.findIndex((h) =>
      /include|cluster/i.test(h)
    );
    const exIdx = m.study_data.headers.findIndex((h) =>
      /exclude/i.test(h)
    );
    if (mkIdx < 0) return m;

    const existing = m.study_data.rows
      .map((r) => (r[mkIdx] || "").trim())
      .filter((v) => v);

    if (existing.length > 0) {
      existed = true;
      return m;
    }

    const nRows = Math.max(
      m.study_data.rows.length,
      includeNames.length,
      excludeNames.length
    );
    while (m.study_data.rows.length < nRows) {
      m.study_data.rows.push(m.study_data.headers.map(() => ""));
    }
    for (let i = 0; i < includeNames.length; i++) {
      m.study_data.rows[i][mkIdx] = includeNames[i];
    }
    if (exIdx >= 0) {
      for (let i = 0; i < excludeNames.length; i++) {
        m.study_data.rows[i][exIdx] = excludeNames[i];
      }
    }
    filled = true;
    return m;
  });

  if (filled) {
    isDirty.set(true);
    const excludeNote =
      excludeNames.length > 0
        ? `, plus ${excludeNames.length} scatter/time channels in "Markers to exclude completely"`
        : "";
    return {
      kind: "info",
      text: `Detected ${includeNames.length} markers from ${sourceFile}${excludeNote}.`,
    };
  }
  if (existed) {
    const totalNames = includeNames.length + excludeNames.length;
    return {
      kind: "info",
      text: `Detected ${totalNames} channels in ${sourceFile}, but the Study Data marker column already has entries — not overwriting.`,
    };
  }
  return null;
}
