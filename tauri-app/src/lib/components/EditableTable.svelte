<script>
  let { headers = $bindable([]), rows = $bindable([]), onchange = () => {}, cellStyle = null, columnConfig = {} } = $props();

  function getCellStyle(rowIdx, colIdx, value) {
    if (!cellStyle) return "";
    const s = cellStyle(rowIdx, colIdx, value);
    if (!s) return "";
    return Object.entries(s).map(([k, v]) => `${k}:${v}`).join(";");
  }

  function addRow() {
    rows = [...rows, headers.map(() => "")];
    onchange();
  }

  function removeRow(index) {
    rows = rows.filter((_, i) => i !== index);
    onchange();
  }

  function addColumn() {
    const name = prompt("Column name:");
    if (!name) return;
    headers = [...headers, name];
    rows = rows.map((row) => [...row, ""]);
    onchange();
  }

  function removeColumn(index) {
    headers = headers.filter((_, i) => i !== index);
    rows = rows.map((row) => row.filter((_, i) => i !== index));
    onchange();
  }

  function handleCellEdit(rowIdx, colIdx, e) {
    rows[rowIdx] = [...rows[rowIdx]];
    rows[rowIdx][colIdx] = e.target.value;
    rows = [...rows];
    onchange();
  }

  function mergeContrast(a, b) {
    a = (a || "").trim();
    b = (b || "").trim();
    if (!a && !b) return "";
    return `${a} over ${b}`;
  }
</script>

<div class="table-wrapper">
  <div class="table-actions">
    <button class="btn-small" onclick={addRow}>+ Row</button>
    <button class="btn-small" onclick={addColumn}>+ Column</button>
  </div>

  <div class="table-scroll">
    <table>
      <thead>
        <tr>
          <th class="row-num">#</th>
          {#each headers as header, colIdx}
            <th>
              <span>{header}</span>
              <button class="remove-col" onclick={() => removeColumn(colIdx)} title="Remove column">&times;</button>
            </th>
          {/each}
          <th class="action-col"></th>
        </tr>
      </thead>
      <tbody>
        {#each rows as row, rowIdx}
          <tr>
            <td class="row-num">{rowIdx + 1}</td>
            {#each row as cell, colIdx}
              <td style={getCellStyle(rowIdx, colIdx, cell)}>
                {#if columnConfig[colIdx]?.type === "contrast"}
                  {@const parts = (cell || "").includes(" over ") ? cell.split(" over ") : ["", ""]}
                  <span class="contrast-cell">
                    <select
                      value={parts[0]?.trim() || ""}
                      onchange={(e) => handleCellEdit(rowIdx, colIdx, { target: { value: mergeContrast(e.target.value, parts[1]) }})}
                    >
                      <option value="">--</option>
                      {#each columnConfig[colIdx].options as opt}
                        <option value={opt}>{opt}</option>
                      {/each}
                    </select>
                    <span class="over-label">over</span>
                    <select
                      value={parts[1]?.trim() || ""}
                      onchange={(e) => handleCellEdit(rowIdx, colIdx, { target: { value: mergeContrast(parts[0], e.target.value) }})}
                    >
                      <option value="">--</option>
                      {#each columnConfig[colIdx].options as opt}
                        <option value={opt}>{opt}</option>
                      {/each}
                    </select>
                  </span>
                {:else if columnConfig[colIdx]?.type === "dropdown"}
                  <select
                    value={cell}
                    onchange={(e) => handleCellEdit(rowIdx, colIdx, e)}
                  >
                    <option value=""></option>
                    {#each columnConfig[colIdx].options as opt}
                      <option value={opt}>{opt}</option>
                    {/each}
                  </select>
                {:else}
                  <input
                    type="text"
                    value={cell}
                    onchange={(e) => handleCellEdit(rowIdx, colIdx, e)}
                  />
                {/if}
              </td>
            {/each}
            <td class="action-col">
              <button class="remove-row" onclick={() => removeRow(rowIdx)} title="Remove row">&times;</button>
            </td>
          </tr>
        {/each}
        {#if rows.length === 0}
          <tr>
            <td colspan={headers.length + 2} class="empty-msg">No data. Click "+ Row" to add.</td>
          </tr>
        {/if}
      </tbody>
    </table>
  </div>
</div>

<style>
  .table-wrapper {
    padding: 1rem;
  }
  .table-actions {
    display: flex;
    gap: 0.5rem;
    margin-bottom: 0.5rem;
  }
  .btn-small {
    padding: 0.25rem 0.6rem;
    font-size: 0.8rem;
    border: 1px solid #ccc;
    border-radius: 4px;
    background: #fff;
    cursor: pointer;
    color: #333;
  }
  .btn-small:hover {
    background: #eff6ff;
    border-color: #2563eb;
  }
  .table-scroll {
    overflow-x: auto;
  }
  table {
    width: 100%;
    border-collapse: collapse;
    font-size: 0.82rem;
  }
  th, td {
    border: 1px solid #ddd;
    padding: 0.3rem 0.4rem;
    text-align: left;
    white-space: nowrap;
  }
  th {
    background: #f0f5ff;
    font-weight: 600;
    color: #555;
    position: relative;
  }
  .row-num {
    width: 30px;
    text-align: center;
    color: #999;
    background: #fafafa;
  }
  .action-col {
    width: 30px;
    text-align: center;
  }
  td input, td select {
    width: 100%;
    min-width: 80px;
    border: none;
    padding: 0.2rem;
    font-size: 0.82rem;
    background: transparent;
    font-family: inherit;
  }
  td input:focus, td select:focus {
    outline: none;
    background: #fffde8;
  }
  .remove-col, .remove-row {
    background: none;
    border: none;
    color: #c44;
    cursor: pointer;
    font-size: 1rem;
    padding: 0 0.2rem;
    opacity: 0.5;
  }
  .remove-col:hover, .remove-row:hover {
    opacity: 1;
  }
  .empty-msg {
    text-align: center;
    color: #999;
    padding: 1rem;
    font-style: italic;
  }
  .contrast-cell {
    display: flex;
    align-items: center;
    gap: 0.3rem;
  }
  .contrast-cell select {
    flex: 1;
    min-width: 100px;
  }
  .over-label {
    color: #94a3b8;
    font-size: 0.75rem;
    font-style: italic;
    flex-shrink: 0;
  }
</style>
