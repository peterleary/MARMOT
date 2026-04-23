use crate::process::runner;

// All commands below that invoke subprocesses (Rscript, quarto, which/where)
// are declared `async fn` and dispatch the blocking work via
// `tauri::async_runtime::spawn_blocking`. Without this, Tauri schedules sync
// commands on the main thread on macOS, and the blocking `.output()` call
// beach-balls the UI while the splash is up.

async fn blocking<F, T>(f: F) -> Result<T, String>
where
    F: FnOnce() -> Result<T, String> + Send + 'static,
    T: Send + 'static,
{
    tauri::async_runtime::spawn_blocking(f)
        .await
        .map_err(|e| format!("background task failed: {}", e))?
}

#[tauri::command]
pub async fn find_rscript() -> Result<String, String> {
    blocking(|| runner::find_rscript().ok_or_else(|| "Rscript not found on this system".to_string())).await
}

/// Like find_rscript but checks ~/.config/marmot/rscript_path.txt first.
/// On repeat launches, skips the stat/which search entirely.
#[tauri::command]
pub async fn find_rscript_cached() -> Result<String, String> {
    blocking(|| {
        if let Some(cached) = runner::load_cached_rscript() {
            return Ok(cached);
        }
        let path = runner::find_rscript().ok_or_else(|| "Rscript not found".to_string())?;
        runner::save_cached_rscript(&path);
        Ok(path)
    }).await
}

/// Single R subprocess: returns (version_string, marmot_installed).
#[tauri::command]
pub async fn get_r_info(rscript_path: String) -> Result<(String, bool), String> {
    blocking(move || runner::get_r_info(&rscript_path)).await
}

/// Combined R probe — (version, marmot_installed, package_status).
/// Used by the splash flow so startup pays one Rscript boot instead of
/// two. `package_status` is the same JSON shape as `query_installed_packages`.
#[tauri::command]
pub async fn get_r_status(rscript_path: String) -> Result<(String, bool, serde_json::Value), String> {
    blocking(move || runner::get_r_status(&rscript_path)).await
}

/// Like find_rscript_cached but for Quarto.
#[tauri::command]
pub async fn find_quarto_cached() -> Result<String, String> {
    blocking(|| {
        if let Some(cached) = runner::load_cached_quarto() {
            return Ok(cached);
        }
        let path = runner::find_quarto().ok_or_else(|| "Quarto not found".to_string())?;
        runner::save_cached_quarto(&path);
        Ok(path)
    }).await
}

/// Returns the Quarto version string (e.g. "1.6.40").
#[tauri::command]
pub async fn get_quarto_info(quarto_path: String) -> Result<String, String> {
    blocking(move || runner::get_quarto_version(&quarto_path)).await
}
