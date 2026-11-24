<script>
  let { headers = $bindable([]), rows = $bindable([]), onchange = () => {}, cellStyle = null, columnConfig = {} } = $props();

  // Column help, drawn by the app (the window shows no native tooltips) and
  // placed outside the table so its scrolling area cannot cut it off
  let helpBox = $state(null);
  function showHelp(event, text) {
    const r = event.currentTarget.getBoundingClientRect();
    helpBox = { text, left: Math.max(8, Math.min(r.left, window.innerWidth - 360)), top: r.bottom + 6 };
  }
  function hideHelp() { helpBox = null; }

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

  function mergeMarkerPair(cellType, marker1, marker2) {
    cellType = (cellType || "").trim();
    marker1 = (marker1 || "").trim();
    marker2 = (marker2 || "").trim();
    if (!cellType && !marker1 && !marker2) return "";
    return `${cellType}: ${marker1} ${marker2}`;
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
              {#if columnConfig[colIdx]?.help}
                <span class="col-help" tabindex="0" role="button" aria-label={columnConfig[colIdx].help}
                  onmouseenter={(e) => showHelp(e, columnConfig[colIdx].help)} onmouseleave={hideHelp}
                  onfocus={(e) => showHelp(e, columnConfig[colIdx].help)} onblur={hideHelp}
                  onclick={(e) => showHelp(e, columnConfig[colIdx].help)}>ⓘ</span>
              {/if}
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
                {:else if columnConfig[colIdx]?.type === "marker_pair"}
                  {@const colonIdx = (cell || "").indexOf(":")}
                  {@const cellType = colonIdx >= 0 ? (cell || "").slice(0, colonIdx).trim() : ""}
                  {@const markersPart = colonIdx >= 0 ? (cell || "").slice(colonIdx + 1).trim() : ""}
                  {@const mpMarkers = markersPart ? markersPart.split(/\s+/) : ["", ""]}
                  <span class="marker-pair-cell">
                    <input
                      type="text"
                      class="mp-text"
                      placeholder="Cell type"
                      value={cellType}
                      onchange={(e) => handleCellEdit(rowIdx, colIdx, { target: { value: mergeMarkerPair(e.target.value, mpMarkers[0], mpMarkers[1]) }})}
                    />
                    <span class="mp-colon">:</span>
                    <select
                      value={mpMarkers[0] || ""}
                      onchange={(e) => handleCellEdit(rowIdx, colIdx, { target: { value: mergeMarkerPair(cellType, e.target.value, mpMarkers[1]) }})}
                    >
                      <option value="">--</option>
                      {#each columnConfig[colIdx].options as opt}
                        <option value={opt}>{opt}</option>
                      {/each}
                    </select>
                    <select
                      value={mpMarkers[1] || ""}
                      onchange={(e) => handleCellEdit(rowIdx, colIdx, { target: { value: mergeMarkerPair(cellType, mpMarkers[0], e.target.value) }})}
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
  {#if helpBox}
    <div class="col-help-box" style="left:{helpBox.left}px; top:{helpBox.top}px">{helpBox.text}</div>
  {/if}
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
  .marker-pair-cell {
    display: flex;
    align-items: center;
    gap: 0.2rem;
  }
  .marker-pair-cell input.mp-text {
    flex: 1;
    min-width: 80px;
    max-width: 120px;
  }
  .marker-pair-cell select {
    flex: 1;
    min-width: 80px;
  }
  .mp-colon {
    color: #94a3b8;
    font-size: 0.75rem;
    font-weight: 600;
    flex-shrink: 0;
  }
  .col-help { margin-left: 0.25rem; color: #64748b; cursor: help; font-size: 0.85em; outline: none; }
  .col-help-box {
    position: fixed; z-index: 1000; width: 22rem;
    padding: 0.5rem 0.65rem; background: #1e293b; color: #f8fafc; border-radius: 6px;
    font-size: 0.75rem; font-weight: 400; line-height: 1.45; white-space: normal; text-align: left;
    box-shadow: 0 4px 14px rgba(0, 0, 0, 0.18);
  }
</style>
