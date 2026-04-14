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
pub fn save_template_to(app: tauri::AppHandle, dest_path: String) -> Result<(), String> {
    let resource_path = app
        .path()
        .resource_dir()
        .map_err(|e| format!("Resource dir not found: {}", e))?
        .join("MARMOT_Metadata.xlsx");

    if !resource_path.exists() {
        return Err("Bundled template not found".into());
    }

    std::fs::copy(&resource_path, &dest_path)
        .map_err(|e| format!("Failed to copy template: {}", e))?;

    Ok(())
}

#[tauri::command]
pub fn create_new_metadata() -> MetadataFile {
    use crate::excel::types::{PipelineSetting, SheetData};

    let default_settings = vec![
        ("clusteringMethodToUse", "MfastPG", "Clustering algorithm: FlowSOM, MfastPG, Mphenograph, Mparc (always available); Rphenograph, PARC (require extras)"),
        ("markersToClusterBy", "all", "Which markers to cluster by: all, type, or state"),
        ("kValuesIWant", "20, 40", "Comma-separated k values for clustering"),
        ("knn", "20", "k-nearest neighbours value (must be in kValuesIWant)"),
        ("dimRedMethodToUse", "UMAP", "Dimension reduction: UMAP, TSNE, Mpacmap (always available); pacmap (requires Python extras)"),
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
        ("viridisColour", "lisbon", "Colour palette. Viridis: magma, inferno, plasma, viridis, cividis, rocket, mako, turbo. Scico: bam, berlin, brocO, corkO, lapaz, lisbon, romaO, vikO. Diverging: BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral. Fun: Marmot, Swiss, Lucky"),
        ("RDataFolder", "", "Path to previous R_files folder to reload (leave blank for fresh run)"),
    ];

    let pipeline_settings: Vec<PipelineSetting> = default_settings
        .into_iter()
        .map(|(var, setting, info)| PipelineSetting {
            variable: var.to_string(),
            setting: setting.to_string(),
            info: info.to_string(),
        })
        .collect();

    // Headers must match what the pipeline QMD reads: it uses literal
    // names like `Marker Type`, `Markers to include`, `Conditions Order`,
    // etc. See inst/pipeline/MARMOT_Pipeline.qmd around line 380-420.
    let study_data = SheetData {
        headers: vec![
            "Conditions Order".to_string(),
            "Cells per condition in UMAPs etc.".to_string(),
            "Conditions To Test".to_string(),
            "Markers to include".to_string(),
            "Marker Type".to_string(),
            "Cofactors for markers to use".to_string(),
            "Markers to exclude completely".to_string(),
            "Marker Pairs".to_string(),
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
