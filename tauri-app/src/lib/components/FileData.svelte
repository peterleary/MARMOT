<script>
  import { invoke } from "@tauri-apps/api/core";
  import { open } from "@tauri-apps/plugin-dialog";
  import { metadata, isDirty, fcsFolder } from "../stores/metadata.js";
  import EditableTable from "./EditableTable.svelte";

  let scanning = $state(false);

  async function scanFolder() {
    try {
      const selected = await open({ directory: true, title: "Select FCS folder" });
      if (!selected) return;

      fcsFolder.set(selected);
      scanning = true;

      const files = await invoke("scan_fcs_folder", { path: selected });

      metadata.update((m) => {
        // Ensure we have the required columns
        if (!m.file_data.headers.includes("file_name")) {
          m.file_data.headers = ["file_name", "sample_id", "condition", ...m.file_data.headers.filter(h => !["file_name", "sample_id", "condition"].includes(h))];
        }

        const fnIdx = m.file_data.headers.indexOf("file_name");
        const sidIdx = m.file_data.headers.indexOf("sample_id");

        // Populate rows from scanned files
        m.file_data.rows = files.map((fname, i) => {
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
    } catch (e) {
      console.error("Scan failed:", e);
    } finally {
      scanning = false;
    }
  }

  function handleChange() {
    isDirty.set(true);
    metadata.update((m) => m);
  }
</script>

<div class="tab-content">
  <div class="file-toolbar">
    <p class="tab-description">
      Define file metadata. <strong>file_name</strong> must match FCS filenames exactly.
    </p>
    <button class="scan-btn" onclick={scanFolder} disabled={scanning}>
      {scanning ? "Scanning..." : "Scan FCS Folder"}
    </button>
  </div>
  <EditableTable
    bind:headers={$metadata.file_data.headers}
    bind:rows={$metadata.file_data.rows}
    onchange={handleChange}
  />
</div>

<style>
  .tab-content {
    height: 100%;
  }
  .file-toolbar {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 0.75rem 1rem 0;
  }
  .tab-description {
    font-size: 0.82rem;
    color: #666;
    margin: 0;
  }
  .scan-btn {
    padding: 0.35rem 0.8rem;
    font-size: 0.82rem;
    border: 1px solid #2563eb;
    border-radius: 4px;
    background: #f0f5ff;
    color: #2563eb;
    cursor: pointer;
    font-weight: 500;
    white-space: nowrap;
  }
  .scan-btn:hover:not(:disabled) {
    background: #dbeafe;
  }
  .scan-btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
</style>
