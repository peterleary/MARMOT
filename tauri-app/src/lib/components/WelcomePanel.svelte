<script>
  import { invoke } from "@tauri-apps/api/core";
  import { save } from "@tauri-apps/plugin-dialog";

  let saving = $state(false);
  let saveMessage = $state("");

  async function handleDownloadTemplate() {
    const destPath = await save({
      title: "Save MARMOT Metadata Template",
      filters: [{ name: "Excel", extensions: ["xlsx"] }],
      defaultPath: "MARMOT_Metadata.xlsx",
    });
    if (!destPath) return;

    saving = true;
    saveMessage = "";
    try {
      await invoke("save_template_to", { destPath });
      saveMessage = "Template saved successfully.";
      setTimeout(() => { saveMessage = ""; }, 4000);
    } catch (e) {
      saveMessage = "Error: " + (e.message || e);
    } finally {
      saving = false;
    }
  }
</script>

<div class="welcome-panel">
  <div class="hero">
    <h1>Welcome to MARMOT</h1>
    <p class="subtitle">Spectral flow cytometry analysis pipeline</p>
  </div>

  <div class="steps">
    <div class="step">
      <span class="step-number">1</span>
      <div class="step-body">
        <strong>Install MARMOT and dependencies</strong>
        <span>
          Go to the <b>Install</b> tab.
          Click <b>Install Packages</b> to install everything &mdash; MARMOT, all R packages, and the Python environment for PARC/PaCMAP.
          All you need beforehand is R and Quarto (which this app already checked for you on startup).
          First-time installs can take 10-20 minutes.
          Use <b>Check Setup</b> afterwards to verify everything is green.
        </span>
      </div>
    </div>

    <div class="step">
      <span class="step-number">2</span>
      <div class="step-body">
        <strong>Load your metadata file</strong>
        <span>
          Click <b>Open</b> in the toolbar above to load the <code>MARMOT_Metadata.xlsx</code> file.
          This file should live in the same folder as your FCS files &mdash; the FCS folder is set automatically when you open it.
          The metadata file contains three sheets:
        </span>
        <ul>
          <li><b>Pipeline Settings</b> &mdash; clustering method, DR, QC, contrasts, etc.</li>
          <li><b>Study Design</b> &mdash; marker panel: <code>fcs_colname</code>, <code>antigen</code>, and <code>marker_class</code> (type, state, or none)</li>
          <li><b>File Data</b> &mdash; one row per FCS file with <code>file_name</code>, <code>sample_id</code>, <code>condition</code>, and any other grouping variables</li>
        </ul>
        <span>
          Don't have a metadata file yet? Download the template to your FCS folder, fill it in, then open it:
        </span>
        <div class="template-action">
          <button
            class="btn-template"
            onclick={handleDownloadTemplate}
            disabled={saving}
          >
            {saving ? "Saving..." : "Download Template"}
          </button>
          {#if saveMessage}
            <span class="save-msg" class:error={saveMessage.startsWith("Error")}>{saveMessage}</span>
          {/if}
        </div>
        <span>
          You can also use <b>Scan FCS Folder</b> in the <b>Files</b> tab to auto-populate filenames.
        </span>
      </div>
    </div>

    <div class="step">
      <span class="step-number">3</span>
      <div class="step-body">
        <strong>Review settings (optional)</strong>
        <span>
          The <b>Settings</b> tab shows all pipeline parameters loaded from your metadata file.
          You can adjust them here if needed &mdash; clustering method, dimensionality reduction, number of clusters, QC, contrasts for DA/DS, and more.
          Any changes you make are saved when you click <b>Save</b> in the toolbar or when you run the pipeline.
        </span>
      </div>
    </div>

    <div class="step">
      <span class="step-number">4</span>
      <div class="step-body">
        <strong>Run the pipeline</strong>
        <span>
          Give your run a name in the toolbar (top-right), then click <b>Run Pipeline</b>.
          The <b>Run/log</b> tab shows live output. A typical run takes 5-30 minutes depending on dataset size.
          When it finishes, the output folder contains an interactive HTML report and an <code>R_files/</code> directory with all results.
        </span>
      </div>
    </div>

    <div class="step">
      <span class="step-number">5</span>
      <div class="step-body">
        <strong>Explore results</strong>
        <span>
          After a successful run, switch to the <b>Shiny</b> tab to launch the interactive explorer.
          This lets you browse DR plots, marker expression, cluster heatmaps, and differential abundance/state results.
        </span>
      </div>
    </div>
  </div>

  <div class="tip-box">
    <strong>Tip:</strong> Marker classes matter. Every marker should be either <b>type</b> or <b>state</b>.
    <b>Type</b> markers (e.g. lineage markers) are used for clustering.
    <b>State</b> markers (e.g. activation markers) are used for differential state analysis.
    To exclude markers entirely, use the <b>excludeTheseMarkers</b> column in the metadata file.
  </div>

  <div class="footer-note">
    For more information and example data, see the
    <a href="https://github.com/peterleary/MARMOT" target="_blank" rel="noopener">MARMOT documentation on GitHub</a>.
  </div>
</div>

<style>
  .welcome-panel {
    display: flex;
    flex-direction: column;
    height: 100%;
    padding: 2rem 2rem 1.5rem;
    gap: 1.25rem;
    overflow-y: auto;
  }

  .hero h1 {
    font-size: 1.35rem;
    font-weight: 700;
    color: #1e293b;
    margin-bottom: 0.25rem;
  }
  .hero .subtitle {
    font-size: 0.88rem;
    color: #64748b;
  }

  .steps {
    display: flex;
    flex-direction: column;
    gap: 0.6rem;
  }
  .step {
    display: flex;
    align-items: flex-start;
    gap: 0.85rem;
    padding: 0.75rem 1rem;
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-radius: 8px;
  }
  .step-number {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 28px;
    height: 28px;
    min-width: 28px;
    background: #2563eb;
    color: #fff;
    font-size: 0.82rem;
    font-weight: 700;
    border-radius: 50%;
    margin-top: 0.1rem;
  }
  .step-body {
    display: flex;
    flex-direction: column;
    gap: 0.2rem;
  }
  .step-body strong {
    font-size: 0.88rem;
    color: #1e293b;
  }
  .step-body span {
    font-size: 0.8rem;
    color: #64748b;
    line-height: 1.5;
  }
  .step-body b {
    color: #334155;
  }
  .step-body code {
    font-size: 0.76rem;
    background: #e2e8f0;
    padding: 0.1rem 0.3rem;
    border-radius: 3px;
    color: #334155;
  }
  .step-body ul {
    margin: 0.25rem 0 0.15rem 1.2rem;
    padding: 0;
    font-size: 0.8rem;
    color: #64748b;
    line-height: 1.55;
  }
  .step-body li {
    margin-bottom: 0.1rem;
  }

  .template-action {
    display: flex;
    align-items: center;
    gap: 0.6rem;
    margin: 0.3rem 0;
  }
  .btn-template {
    padding: 0.4rem 0.85rem;
    border: none;
    border-radius: 5px;
    background: #2563eb;
    color: #fff;
    font-size: 0.8rem;
    font-weight: 600;
    font-family: inherit;
    cursor: pointer;
    transition: background 0.15s;
  }
  .btn-template:hover:not(:disabled) {
    background: #1d4ed8;
  }
  .btn-template:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
  .save-msg {
    font-size: 0.76rem;
    color: #16a34a;
  }
  .save-msg.error {
    color: #dc2626;
  }

  .tip-box {
    font-size: 0.8rem;
    color: #475569;
    background: #eff6ff;
    border: 1px solid #bfdbfe;
    border-radius: 6px;
    padding: 0.65rem 0.9rem;
    line-height: 1.5;
  }
  .tip-box b {
    color: #1e293b;
  }

  .footer-note {
    font-size: 0.78rem;
    color: #94a3b8;
    margin-top: auto;
  }
  .footer-note a {
    color: #2563eb;
    text-decoration: none;
  }
  .footer-note a:hover {
    text-decoration: underline;
  }
</style>
