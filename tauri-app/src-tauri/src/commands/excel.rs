use crate::excel::reader::read_metadata_excel;
use crate::excel::types::MetadataFile;
use crate::excel::writer::write_metadata_excel;
use tauri::Manager;

#[tauri::command]
pub fn load_default_metadata(app: tauri::AppHandle) -> Result<MetadataFile, String> {
    let resource_path = app
        .path()
        .resource_dir()
        .map_err(|e| format!("Resource dir not found: {}", e))?
        .join("MARMOT_Metadata.xlsx");
    read_metadata_excel(&resource_path.to_string_lossy())
}

#[tauri::command]
pub fn read_excel(path: String) -> Result<MetadataFile, String> {
    read_metadata_excel(&path)
}

#[tauri::command]
pub fn write_excel(metadata: MetadataFile, path: String) -> Result<(), String> {
    write_metadata_excel(&metadata, &path)
}

#[tauri::command]
pub fn create_new_metadata() -> MetadataFile {
    use crate::excel::types::{PipelineSetting, SheetData};

    let default_settings = vec![
        ("clusteringMethodToUse", "Rphenograph", "Clustering algorithm: Rphenograph, FastPG, PARC, or FlowSOM"),
        ("markersToClusterBy", "all", "Which markers to cluster by: all, type, or state"),
        ("kValuesIWant", "20, 40", "Comma-separated k values for clustering"),
        ("knn", "20", "k-nearest neighbours value (must be in kValuesIWant)"),
        ("dimRedMethodToUse", "UMAP", "Dimension reduction: TSNE, UMAP, or pacmap"),
        ("markersToDimRedBy", "all", "Which markers for dim. reduction: all, type, or state"),
        ("runQC", "PeacoQC", "QC method: FlowAI, PeacoQC, or None"),
        ("useQC", "FALSE", "Whether to keep only QC-passed cells"),
        ("downsampleTo", "", "Number of cells to downsample to per file, or leave blank for no downsampling"),
        ("gimmePDFs", "FALSE", "Export plots as PDFs"),
        ("greyscalePlots", "FALSE", "Use greyscale for plots"),
        ("quantileNormaliseAll", "FALSE", "Apply quantile normalisation to all markers"),
        ("runScGate", "FALSE", "Run scGate marker-pair gating (requires scGate, UCell, Seurat)"),
        ("runInParallel", "TRUE", "Run pipeline in parallel"),
        ("nCores", "4", "Number of CPU cores for parallel execution"),
        ("ramPerCore", "6", "RAM (GB) allocated per core"),
        ("themeToUse", "prism", "ggplot2 theme: prism, classic, bw, minimal, void, light, dark"),
        ("viridisColour", "lisbon", "Colour palette: lisbon, berlin, vik, cork, batlow, lapaz, magma, inferno, plasma, viridis, mako, rocket, turbo"),
        ("RDataFolder", "", "Path to previous R_files folder to reload (leave blank for fresh run)"),
        ("condaDir", "", "Path to conda/mamba binary"),
        ("parcScriptDir", "", "Path to folder containing f_parc.py and f_pacmap.py"),
    ];

    let pipeline_settings: Vec<PipelineSetting> = default_settings
        .into_iter()
        .map(|(var, setting, info)| PipelineSetting {
            variable: var.to_string(),
            setting: setting.to_string(),
            info: info.to_string(),
        })
        .collect();

    let study_data = SheetData {
        headers: vec![
            "fcs_colname".to_string(),
            "antigen".to_string(),
            "marker_class".to_string(),
        ],
        rows: Vec::new(),
    };

    let file_data = SheetData {
        headers: vec![
            "file_name".to_string(),
            "sample_id".to_string(),
            "condition".to_string(),
        ],
        rows: Vec::new(),
    };

    let options = SheetData {
        headers: vec![
            "condition".to_string(),
            "base_condition".to_string(),
        ],
        rows: Vec::new(),
    };

    MetadataFile {
        path: None,
        pipeline_settings,
        study_data,
        file_data,
        options,
    }
}
