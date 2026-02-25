<script>
  import { metadata, isDirty } from "../stores/metadata.js";
  import EditableTable from "./EditableTable.svelte";

  function handleChange() {
    isDirty.set(true);
    metadata.update((m) => m);
  }
</script>

<div class="tab-content">
  <p class="tab-description">
    Define markers and their classes. Columns: <strong>fcs_colname</strong> (channel name in FCS),
    <strong>antigen</strong> (marker name), <strong>marker_class</strong> (type, state, or none).
  </p>
  <EditableTable
    bind:headers={$metadata.study_data.headers}
    bind:rows={$metadata.study_data.rows}
    onchange={handleChange}
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
</style>
