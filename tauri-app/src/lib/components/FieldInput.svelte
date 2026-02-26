<script>
  let { type = "text", value = $bindable(""), options = [], disabledOptions = [], label = "", info = "", placeholder = "", min = undefined, allowEmpty = false } = $props();

  let fieldId = $derived("field-" + label.replace(/\s+/g, "-").toLowerCase());
  let showTooltip = $state(false);

  function handleCheckbox(e) {
    value = e.target.checked ? "TRUE" : "FALSE";
  }

  let isChecked = $derived(value === "TRUE" || value === "true");
</script>

<div class="field-input">
  <label class="field-label" for={fieldId}>
    <span>{label}</span>
    {#if info}
      <span
        class="info-icon"
        role="button"
        tabindex="-1"
        onmouseenter={() => showTooltip = true}
        onmouseleave={() => showTooltip = false}
        onfocus={() => showTooltip = true}
        onblur={() => showTooltip = false}
      >
        ?
        {#if showTooltip}
          <span class="tooltip">{info}</span>
        {/if}
      </span>
    {/if}
  </label>

  {#if type === "dropdown"}
    <select id={fieldId} bind:value class="field-control">
      {#each options as opt}
        {@const unavailable = disabledOptions.includes(opt)}
        <option value={opt} disabled={unavailable} class:unavailable-opt={unavailable}>
          {opt}{unavailable ? " (not installed)" : ""}
        </option>
      {/each}
    </select>
  {:else if type === "checkbox"}
    <label class="checkbox-wrapper">
      <input type="checkbox" checked={isChecked} onchange={handleCheckbox} />
      <span class="checkmark">{isChecked ? "Yes" : "No"}</span>
    </label>
  {:else if type === "number"}
    <input
      id={fieldId}
      type="number"
      bind:value
      class="field-control"
      {placeholder}
      {min}
    />
  {:else}
    <input
      id={fieldId}
      type="text"
      bind:value
      class="field-control"
      {placeholder}
    />
  {/if}
</div>

<style>
  .field-input {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    margin-bottom: 0.5rem;
  }
  .field-label {
    min-width: 170px;
    font-size: 0.85rem;
    font-weight: 500;
    display: flex;
    align-items: center;
    gap: 0.35rem;
    color: #444;
  }
  .info-icon {
    position: relative;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: #93c5fd;
    color: #fff;
    font-size: 0.6rem;
    font-weight: bold;
    cursor: help;
    flex-shrink: 0;
    transition: background 0.15s;
  }
  .info-icon:hover {
    background: #2563eb;
  }
  .tooltip {
    position: absolute;
    left: calc(100% + 8px);
    top: 50%;
    transform: translateY(-50%);
    background: #1e293b;
    color: #eff6ff;
    padding: 0.45rem 0.65rem;
    border-radius: 5px;
    font-size: 0.78rem;
    font-weight: 400;
    white-space: normal;
    width: 260px;
    line-height: 1.4;
    z-index: 100;
    box-shadow: 0 3px 12px rgba(0,0,0,0.2);
    pointer-events: none;
  }
  .tooltip::before {
    content: "";
    position: absolute;
    right: 100%;
    top: 50%;
    transform: translateY(-50%);
    border: 5px solid transparent;
    border-right-color: #1e293b;
  }
  .field-control {
    flex: 1;
    padding: 0.4rem 0.55rem;
    border: 1px solid #cbd5e1;
    border-radius: 5px;
    font-size: 0.85rem;
    max-width: 300px;
    font-family: inherit;
    background: #fff;
    transition: border-color 0.15s, box-shadow 0.15s;
  }
  .field-control:focus {
    outline: none;
    border-color: #2563eb;
    box-shadow: 0 0 0 2.5px rgba(37, 99, 235, 0.12);
  }
  select.field-control {
    cursor: pointer;
  }
  :global(option.unavailable-opt) {
    color: #9ca3af;
  }
  .checkbox-wrapper {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    cursor: pointer;
    font-size: 0.85rem;
  }
  .checkbox-wrapper input[type="checkbox"] {
    width: 16px;
    height: 16px;
    accent-color: #2563eb;
  }
  .checkmark {
    color: #666;
    font-size: 0.82rem;
  }
</style>
