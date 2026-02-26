use crate::process::runner;

#[tauri::command]
pub fn find_rscript() -> Result<String, String> {
    runner::find_rscript().ok_or_else(|| "Rscript not found on this system".to_string())
}

/// Like find_rscript but checks ~/.config/marmot/rscript_path.txt first.
/// On repeat launches, skips the stat/which search entirely.
#[tauri::command]
pub fn find_rscript_cached() -> Result<String, String> {
    if let Some(cached) = runner::load_cached_rscript() {
        return Ok(cached);
    }
    let path = runner::find_rscript().ok_or_else(|| "Rscript not found".to_string())?;
    runner::save_cached_rscript(&path);
    Ok(path)
}

/// Single R subprocess: returns (version_string, marmot_installed).
/// Replaces sequential get_r_version + check_marmot_installed calls.
#[tauri::command]
pub fn get_r_info(rscript_path: String) -> Result<(String, bool), String> {
    runner::get_r_info(&rscript_path)
}
