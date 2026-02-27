mod commands;
mod excel;
mod process;

use process::runner::{PipelineProcess, SharedProcess, SharedShinyProcess};
use std::sync::{Arc, Mutex};

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    let pipeline_process: SharedProcess = Arc::new(Mutex::new(PipelineProcess::new()));
    let shiny_process: SharedShinyProcess = Arc::new(Mutex::new(None));

    // Clone for the window-event closure; the original moves into .manage()
    let shiny_for_event = shiny_process.clone();

    tauri::Builder::default()
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_shell::init())
        .manage(pipeline_process)
        .manage(shiny_process)
        .invoke_handler(tauri::generate_handler![
            commands::excel::load_default_metadata,
            commands::excel::read_excel,
            commands::excel::write_excel,
            commands::excel::create_new_metadata,
            commands::filesystem::scan_fcs_folder,
            commands::filesystem::find_latest_results_dir,
            commands::filesystem::open_path,
            commands::rscript::find_rscript,
            commands::rscript::find_rscript_cached,
            commands::rscript::get_r_info,
            commands::rscript::find_quarto_cached,
            commands::rscript::get_quarto_info,
            commands::install::run_install,
            commands::install::run_check_setup,
            commands::install::query_installed_packages,
            commands::pipeline::run_pipeline,
            commands::pipeline::cancel_pipeline,
            commands::pipeline::launch_shiny_app,
        ])
        // Option A: kill Shiny when the main window is closed
        .on_window_event(move |_window, event| {
            if let tauri::WindowEvent::Destroyed = event {
                process::runner::kill_shiny(&shiny_for_event);
            }
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
