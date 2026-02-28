use tauri::{AppHandle, State};
use crate::process::runner::{self, SharedProcess, SharedShinyProcess};

#[tauri::command]
pub fn run_pipeline(
    app: AppHandle,
    rscript_path: String,
    metadata_path: String,
    run_name: String,
    process: State<'_, SharedProcess>,
) -> Result<(), String> {
    let proc = process.lock().map_err(|e| format!("Lock error: {}", e))?;
    if proc.running {
        return Err("Pipeline is already running".to_string());
    }
    drop(proc);

    runner::spawn_pipeline(
        app,
        rscript_path,
        metadata_path,
        run_name,
        process.inner().clone(),
    );

    Ok(())
}

#[tauri::command]
pub fn cancel_pipeline(process: State<'_, SharedProcess>) -> Result<(), String> {
    runner::kill_pipeline(process.inner())
}

#[tauri::command]
pub fn launch_shiny_app(
    rscript_path: String,
    r_files_path: String,
    shiny_process: State<'_, SharedShinyProcess>,
) -> Result<(), String> {
    // Option C: kill any existing Shiny instance before starting a new one
    runner::kill_shiny(shiny_process.inner());

    let safe_path = r_files_path
        .replace('\\', "/")
        .replace('"', "\\\"");
    let r_expr = format!(
        "MARMOT::shinyMarmot(marmot_output=\"{}\")",
        safe_path
    );

    let enriched_path = runner::enrich_path();

    let mut cmd = std::process::Command::new(&rscript_path);
    cmd.args(["-e", &r_expr])
        .env("PATH", &enriched_path)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null());

    // Put in its own process group so kill_shiny can clean up any R children
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        cmd.process_group(0);
    }

    let child = cmd.spawn()
        .map_err(|e| format!("Failed to launch Shiny app: {}", e))?;

    // Store PID. Dropping Child here does NOT kill the process (Rust stdlib guarantee).
    let pid = child.id();
    *shiny_process.lock().map_err(|e| format!("Lock error: {}", e))? = Some(pid);

    Ok(())
}
