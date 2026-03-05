use std::collections::HashMap;
use std::fs;

#[tauri::command]
pub fn read_reload_settings(path: String) -> Result<HashMap<String, String>, String> {
    let json_path = std::path::Path::new(&path).join("pipeline_settings.json");
    if !json_path.exists() {
        return Err("No pipeline_settings.json found".into());
    }
    let content = fs::read_to_string(&json_path).map_err(|e| e.to_string())?;
    serde_json::from_str(&content).map_err(|e| e.to_string())
}

#[tauri::command]
pub fn find_latest_results_dir(fcs_folder: String) -> Result<String, String> {
    let entries = std::fs::read_dir(&fcs_folder)
        .map_err(|e| format!("Cannot read folder: {}", e))?;

    let mut dirs: Vec<(std::path::PathBuf, std::time::SystemTime)> = entries
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.file_name().to_string_lossy().starts_with("Results_Files_")
                && e.path().is_dir()
        })
        .filter_map(|e| {
            let path = e.path();
            let mtime = e.metadata().ok()?.modified().ok()?;
            Some((path, mtime))
        })
        .collect();

    dirs.sort_by(|a, b| b.1.cmp(&a.1));

    dirs.into_iter()
        .next()
        .map(|(p, _)| p.to_string_lossy().to_string())
        .ok_or_else(|| "No Results_Files_* directory found in the FCS folder".to_string())
}

#[tauri::command]
pub fn open_path(path: String) -> Result<(), String> {
    #[cfg(target_os = "macos")]
    let cmd = "open";
    #[cfg(target_os = "linux")]
    let cmd = "xdg-open";
    #[cfg(target_os = "windows")]
    let cmd = "explorer";

    std::process::Command::new(cmd)
        .arg(&path)
        .spawn()
        .map_err(|e| format!("Failed to open '{}': {}", path, e))?;
    Ok(())
}

#[tauri::command]
pub fn scan_fcs_folder(path: String) -> Result<Vec<String>, String> {
    let entries = fs::read_dir(&path)
        .map_err(|e| format!("Failed to read directory '{}': {}", path, e))?;

    let mut fcs_files: Vec<String> = entries
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let name = entry.file_name().to_string_lossy().to_string();
            if name.to_lowercase().ends_with(".fcs") {
                Some(name)
            } else {
                None
            }
        })
        .collect();

    fcs_files.sort_by(|a, b| a.to_lowercase().cmp(&b.to_lowercase()));
    Ok(fcs_files)
}
