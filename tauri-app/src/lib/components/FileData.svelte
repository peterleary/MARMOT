<script>
  import { open } from "@tauri-apps/plugin-dialog";
  import { metadata, isDirty } from "../stores/metadata.js";
  import { scanFcsFolder } from "../utils/fcs-scan.js";
  import EditableTable from "./EditableTable.svelte";

  let scanning = $state(false);

  // Pastel palette for condition colour-coding
  const PALETTE = [
    "#dbeafe", "#dcfce7", "#fef3c7", "#fce7f3", "#e0e7ff",
    "#d1fae5", "#fee2e2", "#f3e8ff", "#ccfbf1", "#fef9c3",
    "#e8d5f5", "#cfe2f3", "#f4cccc", "#d9ead3", "#fff2cc",
  ];

  let conditionColourMap = $derived.by(() => {
    const condIdx = $metadata.file_data.headers.indexOf("condition");
    if (condIdx < 0) return new Map();
    const map = new Map();
    for (const row of $metadata.file_data.rows) {
      const v = (row[condIdx] || "").trim();
      if (v && !map.has(v)) {
        map.set(v, PALETTE[map.size % PALETTE.length]);
      }
    }
    return map;
  });

  function cellStyle(rowIdx, colIdx, value) {
    const condIdx = $metadata.file_data.headers.indexOf("condition");
    if (colIdx !== condIdx || !value || !value.trim()) return null;
    return { "background-color": conditionColourMap.get(value.trim()) };
  }

  // Detect near-duplicate conditions (differ only in case)
  let caseWarnings = $derived.by(() => {
    const condIdx = $metadata.file_data.headers.indexOf("condition");
    if (condIdx < 0) return [];
    const values = $metadata.file_data.rows
      .map(r => (r[condIdx] || "").trim())
      .filter(v => v);
    const unique = [...new Set(values)];
    const warnings = [];
    for (let i = 0; i < unique.length; i++) {
      for (let j = i + 1; j < unique.length; j++) {
        if (unique[i].toLowerCase() === unique[j].toLowerCase()) {
          warnings.push([unique[i], unique[j]]);
        }
      }
    }
    return warnings;
  });

  let markerInfoMsg = $state(null);

  async function scanFolder() {
    try {
      const selected = await open({ directory: true, title: "Select FCS folder" });
      if (!selected) return;
      scanning = true;
      markerInfoMsg = null;
      markerInfoMsg = await scanFcsFolder(selected);
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
  {#if markerInfoMsg}
    <div class="marker-info-wrap">
      <div class="marker-info marker-info-{markerInfoMsg.kind}">
        {markerInfoMsg.text}
      </div>
    </div>
  {/if}
  {#if caseWarnings.length > 0}
    <div class="case-warnings">
      {#each caseWarnings as [a, b]}
        <div class="case-warning">
          <strong>Possible typo:</strong> "<span class="cw-val">{a}</span>" and "<span class="cw-val">{b}</span>" differ only in capitalisation. The pipeline treats these as different conditions.
        </div>
      {/each}
    </div>
  {/if}
  <EditableTable
    bind:headers={$metadata.file_data.headers}
    bind:rows={$metadata.file_data.rows}
    onchange={handleChange}
    {cellStyle}
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
  .case-warnings {
    padding: 0.5rem 1rem;
  }
  .case-warning {
    padding: 0.4rem 0.6rem;
    margin-bottom: 0.3rem;
    font-size: 0.8rem;
    background: #fef3c7;
    border: 1px solid #f59e0b;
    border-radius: 4px;
    color: #92400e;
  }
  .cw-val {
    font-family: monospace;
    font-weight: 600;
  }
  .marker-info-wrap {
    padding: 0.5rem 1rem 0;
  }
  .marker-info {
    padding: 0.4rem 0.7rem;
    font-size: 0.8rem;
    border-radius: 4px;
    border-left: 3px solid;
  }
  .marker-info-info {
    background: #eff6ff;
    border-left-color: #2563eb;
    color: #1e3a8a;
  }
  .marker-info-warn {
    background: #fef3c7;
    border-left-color: #f59e0b;
    color: #92400e;
  }
</style>
