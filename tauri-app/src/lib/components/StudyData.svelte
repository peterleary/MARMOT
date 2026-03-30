<script>
  import { metadata, isDirty } from "../stores/metadata.js";
  import EditableTable from "./EditableTable.svelte";

  // ── Condition list from File Data ──────────────────────────────
  let conditions = $derived.by(() => {
    const condIdx = $metadata.file_data.headers.indexOf("condition");
    if (condIdx < 0) return [];
    const vals = $metadata.file_data.rows
      .map(r => (r[condIdx] || "").trim())
      .filter(v => v);
    return [...new Set(vals)];
  });

  // ── Column config: dropdowns for specific columns ─────────────
  let columnConfig = $derived.by(() => {
    const cfg = {};
    for (let i = 0; i < $metadata.study_data.headers.length; i++) {
      const h = $metadata.study_data.headers[i].toLowerCase();
      if (h.includes("conditions to test") && conditions.length > 0) {
        cfg[i] = { type: "contrast", options: conditions };
      } else if (h.includes("conditions order") && conditions.length > 0) {
        cfg[i] = { type: "dropdown", options: conditions };
      }
    }
    return cfg;
  });

  function handleChange() {
    isDirty.set(true);
    metadata.update((m) => m);
  }
</script>

<div class="tab-content">
  <p class="tab-description">
    Define markers, conditions, and contrasts.
    {#if conditions.length > 0}
      <span class="cond-count">{conditions.length} conditions detected from Files tab.</span>
    {:else}
      <span class="cond-missing">Add conditions in the Files tab first to enable dropdowns.</span>
    {/if}
  </p>
  <EditableTable
    bind:headers={$metadata.study_data.headers}
    bind:rows={$metadata.study_data.rows}
    onchange={handleChange}
    {columnConfig}
  />
</div>

<style>
  .tab-content {
    height: 100%;
  }
  .tab-description {
    padding: 0.75rem 1rem 0;
    font-size: 0.82rem;
    color: #666;
    margin: 0;
  }
  .cond-count {
    color: #16a34a;
    font-weight: 500;
  }
  .cond-missing {
    color: #d97706;
    font-weight: 500;
  }
</style>
