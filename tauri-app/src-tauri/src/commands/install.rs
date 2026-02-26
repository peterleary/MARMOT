use std::io::{BufRead, BufReader};
use std::process::Stdio;
use tauri::{AppHandle, Emitter};
use crate::process::runner::enrich_path;

/// Internal helper: spawn Rscript with `r_expr`, stream lines to "install-log",
/// emit "install-done" on completion.
fn spawn_r_expr(app: AppHandle, rscript_path: String, r_expr: String) {
    std::thread::spawn(move || {
        let enriched_path = enrich_path();
        let mut child = match std::process::Command::new(&rscript_path)
            .args(["-e", &r_expr])
            .env("PATH", &enriched_path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(c) => c,
            Err(e) => {
                let _ = app.emit("install-log", format!("ERROR: {}", e));
                let _ = app.emit("install-done", serde_json::json!({"success": false}));
                return;
            }
        };

        if let Some(stdout) = child.stdout.take() {
            let app2 = app.clone();
            std::thread::spawn(move || {
                for line in BufReader::new(stdout).lines().flatten() {
                    let _ = app2.emit("install-log", line);
                }
            });
        }
        if let Some(stderr) = child.stderr.take() {
            let app2 = app.clone();
            std::thread::spawn(move || {
                for line in BufReader::new(stderr).lines().flatten() {
                    let _ = app2.emit("install-log", line);
                }
            });
        }

        match child.wait() {
            Ok(status) => {
                let ok = status.success();
                let _ = app.emit(
                    "install-log",
                    if ok {
                        "Done.".to_string()
                    } else {
                        format!("Exited with code {}", status.code().unwrap_or(-1))
                    },
                );
                let _ = app.emit("install-done", serde_json::json!({"success": ok}));
            }
            Err(e) => {
                let _ = app.emit("install-log", format!("Error: {}", e));
                let _ = app.emit("install-done", serde_json::json!({"success": false}));
            }
        }
    });
}

/// Install MARMOT dependencies.
/// If MARMOT is not yet installed, bootstraps it via pak first.
#[tauri::command]
pub fn run_install(
    app: AppHandle,
    rscript_path: String,
    include_suggests: bool,
    include_python: bool,
) -> Result<(), String> {
    let r_expr = format!(
        "options(repos=c(CRAN='https://cloud.r-project.org')); \
if (!requireNamespace('MARMOT',quietly=TRUE)) {{ \
  if (!requireNamespace('pak',quietly=TRUE)) install.packages('pak'); \
  pak::pkg_install('peterleary/MARMOT',ask=FALSE,dependencies=TRUE) \
}}; \
MARMOT::install_dependencies(include_suggests={suggests},include_python={python})",
        suggests = if include_suggests { "TRUE" } else { "FALSE" },
        python = if include_python { "TRUE" } else { "FALSE" },
    );
    spawn_r_expr(app, rscript_path, r_expr);
    Ok(())
}

/// Run MARMOT::check_setup() and stream its output to the install log.
#[tauri::command]
pub fn run_check_setup(app: AppHandle, rscript_path: String) -> Result<(), String> {
    spawn_r_expr(app, rscript_path, "MARMOT::check_setup()".to_string());
    Ok(())
}

/// Quick synchronous check of which optional packages are available.
/// Returns a JSON object with bool flags for each optional package/feature.
#[tauri::command]
pub fn query_installed_packages(rscript_path: String) -> serde_json::Value {
    let fallback = serde_json::json!({
        "FastPG": false, "PeacoQC": false, "flowAI": false,
        "PARC": false, "pacmap": false
    });

    // Use reticulate::use_condaenv() to load the conda Python in-process, then
    // test imports via py_run_string(). Loading in-process avoids macOS SIP
    // stripping DYLD_LIBRARY_PATH in subprocesses, which breaks llvmlite/numba.
    let r_expr = r#"
fast_pg <- requireNamespace('FastPG', quietly=TRUE)
peacoqc <- requireNamespace('PeacoQC', quietly=TRUE)
flow_ai <- requireNamespace('flowAI', quietly=TRUE)
py <- tryCatch({
  cnd  <- reticulate::conda_binary()
  envs <- reticulate::conda_list(conda = cnd)
  if (!('p4r' %in% envs$name)) stop('p4r env not found')
  reticulate::use_condaenv('p4r', conda = cnd, required = FALSE)
  parc_ok <- tryCatch({ reticulate::py_run_string('import parc',   convert=FALSE); TRUE }, error=function(e) FALSE)
  pcm_ok  <- tryCatch({ reticulate::py_run_string('import pacmap', convert=FALSE); TRUE }, error=function(e) FALSE)
  list(PARC = parc_ok, pacmap = pcm_ok)
}, error = function(e) list(PARC = FALSE, pacmap = FALSE))
pairs <- c(
  paste0('"FastPG":',  tolower(fast_pg)),
  paste0('"PeacoQC":', tolower(peacoqc)),
  paste0('"flowAI":',  tolower(flow_ai)),
  paste0('"PARC":',    tolower(py$PARC)),
  paste0('"pacmap":',  tolower(py$pacmap))
)
cat('MARMOT_PKG_STATUS:{', paste(pairs, collapse = ','), '}', sep = '')
"#;

    let enriched_path = enrich_path();
    let output = std::process::Command::new(&rscript_path)
        .args(["-e", r_expr])
        .env("PATH", &enriched_path)
        .output();

    match output {
        Ok(out) => {
            let stdout = String::from_utf8_lossy(&out.stdout);
            // Use the sentinel prefix to locate our JSON reliably,
            // ignoring any other output R or Python may have written.
            let sentinel = "MARMOT_PKG_STATUS:";
            if let Some(pos) = stdout.find(sentinel) {
                let after = &stdout[pos + sentinel.len()..];
                if let (Some(s), Some(e)) = (after.find('{'), after.find('}')) {
                    let json_str = &after[s..=e];
                    if let Ok(val) = serde_json::from_str::<serde_json::Value>(json_str) {
                        return val;
                    }
                }
            }
            fallback
        }
        Err(_) => fallback,
    }
}
