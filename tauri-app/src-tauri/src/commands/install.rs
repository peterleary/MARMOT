use std::io::{BufRead, BufReader};
use std::process::Stdio;
use tauri::{AppHandle, Emitter};
use crate::process::runner::enrich_path;

/// Internal helper: spawn a command, stream lines to `event_log`, emit `event_done` on completion.
fn spawn_streamed(
    app: AppHandle,
    program: String,
    args: Vec<String>,
    event_log: &'static str,
    event_done: &'static str,
) {
    std::thread::spawn(move || {
        let enriched_path = enrich_path();
        let mut child = match std::process::Command::new(&program)
            .args(&args)
            .env("PATH", &enriched_path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(c) => c,
            Err(e) => {
                let _ = app.emit(event_log, format!("ERROR: {}", e));
                let _ = app.emit(event_done, serde_json::json!({"success": false}));
                return;
            }
        };

        if let Some(stdout) = child.stdout.take() {
            let app2 = app.clone();
            std::thread::spawn(move || {
                for line in BufReader::new(stdout).lines().flatten() {
                    let _ = app2.emit(event_log, line);
                }
            });
        }
        if let Some(stderr) = child.stderr.take() {
            let app2 = app.clone();
            std::thread::spawn(move || {
                for line in BufReader::new(stderr).lines().flatten() {
                    let _ = app2.emit(event_log, line);
                }
            });
        }

        match child.wait() {
            Ok(status) => {
                let ok = status.success();
                let _ = app.emit(
                    event_log,
                    if ok {
                        "Done.".to_string()
                    } else {
                        format!("Exited with code {}", status.code().unwrap_or(-1))
                    },
                );
                let _ = app.emit(event_done, serde_json::json!({"success": ok}));
            }
            Err(e) => {
                let _ = app.emit(event_log, format!("Error: {}", e));
                let _ = app.emit(event_done, serde_json::json!({"success": false}));
            }
        }
    });
}

/// Internal helper: spawn Rscript with `r_expr`, stream lines to "install-log",
/// emit "install-done" on completion.
fn spawn_r_expr(app: AppHandle, rscript_path: String, r_expr: String) {
    spawn_streamed(
        app,
        rscript_path,
        vec!["-e".to_string(), r_expr],
        "install-log",
        "install-done",
    );
}

/// Install the MARMOT R package from GitHub (dev branch).
#[tauri::command]
pub fn run_install_marmot(app: AppHandle, rscript_path: String) -> Result<(), String> {
    let r_expr = "options(repos=c(CRAN='https://cloud.r-project.org')); \
if (!requireNamespace('pak',quietly=TRUE)) install.packages('pak'); \
pak::pkg_install('peterleary/MARMOT@dev',ask=FALSE); \
cat('\\nMARMOT installed successfully.\\n')".to_string();
    spawn_r_expr(app, rscript_path, r_expr);
    Ok(())
}

/// Install extras on top of MARMOT (optional R packages + Python env).
#[tauri::command]
pub fn run_install_extras(
    app: AppHandle,
    rscript_path: String,
    include_suggests: bool,
    include_python: bool,
) -> Result<(), String> {
    let r_expr = format!(
        "if (!requireNamespace('MARMOT',quietly=TRUE)) {{ \
  stop('MARMOT is not installed yet. Click \"Install MARMOT\" first.') \
}}; \
MARMOT::install_marmot_extras(include_suggests={suggests},include_python={python})",
        suggests = if include_suggests { "TRUE" } else { "FALSE" },
        python = if include_python { "TRUE" } else { "FALSE" },
    );
    spawn_r_expr(app, rscript_path, r_expr);
    Ok(())
}

/// Run MARMOT::check_setup() and stream its output to the install log.
#[tauri::command]
pub fn run_check_setup(app: AppHandle, rscript_path: String) -> Result<(), String> {
    spawn_r_expr(app, rscript_path,
        "if (!requireNamespace('MARMOT', quietly=TRUE)) { cat('MARMOT is not installed yet.\\nClick \"Install Packages\" below to install it.\\n') } else { MARMOT::check_setup() }".to_string());
    Ok(())
}

/// Quick synchronous check of which optional packages are available.
/// Returns a JSON object with bool flags for each optional package/feature.
#[tauri::command]
pub fn query_installed_packages(rscript_path: String) -> serde_json::Value {
    let fallback = serde_json::json!({
        "Rphenograph": false, "PeacoQC": false, "flowAI": false,
        "PARC": false, "pacmap": false
    });

    // Check if basilisk env exists (fast path check) then verify imports.
    // obtainEnvironmentPath returns the path without triggering env creation,
    // so this won't block for minutes if the user hasn't run install_marmot_extras yet.
    let r_expr = r#"
rphenograph <- requireNamespace('Rphenograph', quietly=TRUE)
peacoqc <- requireNamespace('PeacoQC', quietly=TRUE)
flow_ai <- requireNamespace('flowAI', quietly=TRUE)
py <- tryCatch({
  p4r <- suppressWarnings(getFromNamespace('p4r_env', 'MARMOT'))
  env_path <- basilisk::obtainEnvironmentPath(p4r)
  if (!dir.exists(env_path)) stop('env not created yet')
  basilisk::basiliskRun(env = p4r, fun = function() {
    parc_ok <- tryCatch({ reticulate::py_run_string('import parc',   convert=FALSE); TRUE }, error=function(e) FALSE)
    pcm_ok  <- tryCatch({ reticulate::py_run_string('import pacmap', convert=FALSE); TRUE }, error=function(e) FALSE)
    list(PARC = parc_ok, pacmap = pcm_ok)
  })
}, error = function(e) list(PARC = FALSE, pacmap = FALSE))
pairs <- c(
  paste0('"Rphenograph":', tolower(rphenograph)),
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

/// Install Quarto automatically using the platform's package manager.
/// Streams progress to "quarto-install-log", emits "quarto-install-done".
/// Returns Err("no_method") if no suitable package manager is found.
#[tauri::command]
pub fn install_quarto(app: AppHandle) -> Result<(), String> {
    let enriched_path = enrich_path();

    #[cfg(target_os = "macos")]
    {
        // Check if brew is available
        let brew = std::process::Command::new("which")
            .arg("brew")
            .env("PATH", &enriched_path)
            .output();
        match brew {
            Ok(out) if out.status.success() => {
                let brew_path = String::from_utf8_lossy(&out.stdout).trim().to_string();
                let _ = app.emit("quarto-install-log", "Found Homebrew, installing Quarto...");
                spawn_streamed(
                    app,
                    brew_path,
                    vec!["install".to_string(), "--cask".to_string(), "quarto".to_string()],
                    "quarto-install-log",
                    "quarto-install-done",
                );
                Ok(())
            }
            _ => Err("no_method".to_string()),
        }
    }

    #[cfg(target_os = "linux")]
    {
        // Check if curl is available
        let curl = std::process::Command::new("which")
            .arg("curl")
            .env("PATH", &enriched_path)
            .output();
        match curl {
            Ok(out) if out.status.success() => {
                let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
                let script = format!(
                    r#"set -e
echo "Detecting latest Quarto release..."
REDIR=$(curl -sI https://github.com/quarto-dev/quarto-cli/releases/latest 2>/dev/null | grep -i '^location:' | tr -d '\r')
VER=$(echo "$REDIR" | grep -oP 'v\K[0-9]+\.[0-9]+\.[0-9]+')
if [ -z "$VER" ]; then
  echo "ERROR: Could not detect latest Quarto version"
  exit 1
fi
echo "Downloading Quarto v$VER..."
ARCH=$(uname -m)
if [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then
  SUFFIX="linux-arm64"
else
  SUFFIX="linux-amd64"
fi
URL="https://github.com/quarto-dev/quarto-cli/releases/download/v$VER/quarto-$VER-$SUFFIX.tar.gz"
mkdir -p "{home}/.local/opt" "{home}/.local/bin"
curl -L "$URL" | tar xz -C "{home}/.local/opt/"
ln -sf "{home}/.local/opt/quarto-$VER/bin/quarto" "{home}/.local/bin/quarto"
echo "Quarto v$VER installed to {home}/.local/bin/quarto"
"#,
                    home = home
                );
                let _ = app.emit("quarto-install-log", "Installing Quarto from GitHub releases...");
                spawn_streamed(
                    app,
                    "sh".to_string(),
                    vec!["-c".to_string(), script],
                    "quarto-install-log",
                    "quarto-install-done",
                );
                Ok(())
            }
            _ => Err("no_method".to_string()),
        }
    }

    #[cfg(target_os = "windows")]
    {
        // Check if winget is available
        let winget = std::process::Command::new("where")
            .arg("winget")
            .env("PATH", &enriched_path)
            .output();
        match winget {
            Ok(out) if out.status.success() => {
                let _ = app.emit("quarto-install-log", "Installing Quarto via winget...");
                spawn_streamed(
                    app,
                    "winget".to_string(),
                    vec![
                        "install".to_string(),
                        "--id".to_string(),
                        "Posit.Quarto".to_string(),
                        "--source".to_string(),
                        "winget".to_string(),
                        "--accept-package-agreements".to_string(),
                        "--accept-source-agreements".to_string(),
                    ],
                    "quarto-install-log",
                    "quarto-install-done",
                );
                Ok(())
            }
            _ => Err("no_method".to_string()),
        }
    }

    #[cfg(not(any(target_os = "macos", target_os = "linux", target_os = "windows")))]
    {
        Err("no_method".to_string())
    }
}
