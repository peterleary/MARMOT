<script>
  import { metadata, updateSetting, getSettingValue } from "../stores/metadata.js";
  import { packageStatus } from "../stores/pipeline.js";
  import { FIELD_DEFINITIONS, SETTING_GROUPS, PACKAGE_REQUIREMENTS } from "../utils/defaults.js";
  import FieldInput from "./FieldInput.svelte";

  let currentMetadata = $derived($metadata);
  let groupedFields = $derived(SETTING_GROUPS.flatMap(g => g.fields));
  let ungrouped = $derived(currentMetadata.pipeline_settings.filter(s => !groupedFields.includes(s.variable)));

  // Auto-inject missing defined fields so they always appear in the UI,
  // even when loaded metadata doesn't include them.
  $effect(() => {
    const existing = new Set(currentMetadata.pipeline_settings.map(s => s.variable));
    const missing = groupedFields.filter(f => FIELD_DEFINITIONS[f] && !existing.has(f));
    if (missing.length > 0) {
      metadata.update(m => {
        for (const f of missing) {
          m.pipeline_settings.push({
            variable: f,
            setting: "",
            info: FIELD_DEFINITIONS[f].placeholder || "",
          });
        }
        return m;
      });
    }
  });

  // Build a map of field → [disabled option values] from current package status
  let disabledByField = $derived(
    Object.entries(PACKAGE_REQUIREMENTS).reduce((acc, [pkg, { field, option }]) => {
      if (!$packageStatus[pkg]) {
        if (!acc[field]) acc[field] = [];
        acc[field].push(option);
      }
      return acc;
    }, {})
  );
</script>

<div class="settings-panel">
  {#each SETTING_GROUPS as group}
    <fieldset class="settings-group">
      <legend>{group.label}</legend>
      {#each group.fields as field}
        {@const def = FIELD_DEFINITIONS[field]}
        {@const setting = currentMetadata.pipeline_settings.find(s => s.variable === field)}
        {#if def && setting}
          <FieldInput
            type={def.type}
            bind:value={setting.setting}
            options={def.options || []}
            disabledOptions={disabledByField[field] || []}
            label={def.label || field}
            info={setting.info || ""}
            placeholder={def.placeholder || ""}
            min={def.min}
            allowEmpty={def.allowEmpty || false}
            onchange={(v) => updateSetting(field, v)}
          />
        {/if}
      {/each}
    </fieldset>
  {/each}

  {#if ungrouped.length > 0}
    <fieldset class="settings-group">
      <legend>Other</legend>
      {#each ungrouped as setting}
        <FieldInput
          type="text"
          bind:value={setting.setting}
          label={setting.variable}
          info={setting.info}
        />
      {/each}
    </fieldset>
  {/if}
</div>

<style>
  .settings-panel {
    padding: 1.1rem 1.2rem;
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(420px, 1fr));
    gap: 0.75rem;
  }
  .settings-group {
    border: 1px solid #dbeafe;
    border-radius: 8px;
    padding: 0.8rem 1rem 0.6rem;
    background: #f8fafc;
    transition: box-shadow 0.15s;
  }
  .settings-group:hover {
    box-shadow: 0 1px 6px rgba(37, 99, 235, 0.06);
  }
  .settings-group legend {
    font-weight: 600;
    font-size: 0.88rem;
    color: #2563eb;
    padding: 0 0.5rem;
    letter-spacing: 0.01em;
  }
</style>
