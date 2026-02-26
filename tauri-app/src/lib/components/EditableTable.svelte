<script>
  let { headers = $bindable([]), rows = $bindable([]), onchange = () => {} } = $props();

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
    rows[rowIdx][colIdx] = e.target.value;
    rows = rows;
    onchange();
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
              <td>
                <input
                  type="text"
                  value={cell}
                  onchange={(e) => handleCellEdit(rowIdx, colIdx, e)}
                />
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
  td input {
    width: 100%;
    min-width: 80px;
    border: none;
    padding: 0.2rem;
    font-size: 0.82rem;
    background: transparent;
    font-family: inherit;
  }
  td input:focus {
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
</style>
