use std::process::{Command, Stdio};
use std::io::{BufRead, BufReader};
use std::sync::{Arc, Mutex};
use tauri::{AppHandle, Emitter};

/// Build an enriched PATH that includes common tool locations.
/// macOS GUI apps inherit a minimal PATH (/usr/bin:/bin:/usr/sbin:/sbin),
/// missing Homebrew, Quarto, conda, user-local bins, etc.
pub(crate) fn enrich_path() -> String {
    let current = std::env::var("PATH").unwrap_or_default();
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());

    let extra_dirs: Vec<String> = vec![
        // Quarto
        "/Applications/quarto/bin".to_string(),
        // Homebrew (Apple Silicon + Intel)
        "/opt/homebrew/bin".to_string(),
        "/usr/local/bin".to_string(),
        // User-local
        format!("{}/.local/bin", home),
        format!("{}/bin", home),
        // Conda / Miniforge / Mambaforge
        format!("{}/miniforge3/bin", home),
        format!("{}/mambaforge/bin", home),
        format!("{}/miniconda3/bin", home),
        format!("{}/anaconda3/bin", home),
        // Cargo
        format!("{}/.cargo/bin", home),
    ];

    let mut parts: Vec<&str> = current.split(':').collect();
    for dir in &extra_dirs {
        if !parts.contains(&dir.as_str()) && std::path::Path::new(dir).is_dir() {
            parts.push(dir);
        }
    }
    parts.join(":")
}

#[derive(Clone)]
pub struct PipelineProcess {
    pub pid: Option<u32>,
    pub running: bool,
}

impl PipelineProcess {
    pub fn new() -> Self {
        Self {
            pid: None,
            running: false,
        }
    }
}

pub type SharedProcess = Arc<Mutex<PipelineProcess>>;

/// Tracks a single optional background process (PID only).
/// Used for the Shiny app so we can kill it when the window closes
/// or when a new instance is requested.
pub type SharedShinyProcess = Arc<Mutex<Option<u32>>>;

/// Kill the Shiny process (and its process group on Unix) if one is running.
/// Clears the stored PID atomically so double-calls are safe.
pub fn kill_shiny(process: &SharedShinyProcess) {
    let pid = match process.lock() {
        Ok(mut guard) => guard.take(),
        Err(_) => return,
    };
    if let Some(pid) = pid {
        #[cfg(unix)]
        unsafe {
            libc::kill(-(pid as libc::pid_t), libc::SIGTERM);
        }
        #[cfg(windows)]
        {
            let _ = Command::new("taskkill")
                .args(["/T", "/F", "/PID", &pid.to_string()])
                .output();
        }
    }
}

pub fn find_quarto() -> Option<String> {
    let candidates = if cfg!(target_os = "macos") {
        vec![
            "/Applications/quarto/bin/quarto",
            "/opt/homebrew/bin/quarto",
            "/usr/local/bin/quarto",
        ]
    } else if cfg!(target_os = "windows") {
        vec![
            r"C:\Program Files\Quarto\bin\quarto.exe",
        ]
    } else {
        vec!["/usr/bin/quarto", "/usr/local/bin/quarto"]
    };

    for path in &candidates {
        if std::path::Path::new(path).exists() {
            return Some(path.to_string());
        }
    }

    let cmd = if cfg!(target_os = "windows") { "where" } else { "which" };
    if let Ok(output) = Command::new(cmd).arg("quarto").output() {
        if output.status.success() {
            let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !path.is_empty() {
                return Some(path);
            }
        }
    }

    None
}

pub fn get_quarto_version(quarto_path: &str) -> Result<String, String> {
    let output = Command::new(quarto_path)
        .arg("--version")
        .env("PATH", enrich_path())
        .output()
        .map_err(|e| e.to_string())?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    } else {
        Err("quarto --version failed".to_string())
    }
}

fn quarto_cache_path() -> std::path::PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    std::path::Path::new(&home)
        .join(".config").join("marmot").join("quarto_path.txt")
}

pub fn load_cached_quarto() -> Option<String> {
    let cached = std::fs::read_to_string(quarto_cache_path()).ok()?.trim().to_string();
    if std::path::Path::new(&cached).exists() { Some(cached) } else { None }
}

pub fn save_cached_quarto(path: &str) {
    if let Some(p) = quarto_cache_path().parent() {
        let _ = std::fs::create_dir_all(p);
    }
    let _ = std::fs::write(quarto_cache_path(), path);
}

pub fn find_rscript() -> Option<String> {
    // Platform-specific known paths
    let candidates = if cfg!(target_os = "macos") {
        vec![
            "/opt/homebrew/bin/Rscript",
            "/usr/local/bin/Rscript",
            "/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript",
        ]
    } else if cfg!(target_os = "windows") {
        vec![
            r"C:\Program Files\R\R-4.4.1\bin\Rscript.exe",
            r"C:\Program Files\R\R-4.4.0\bin\Rscript.exe",
            r"C:\Program Files\R\R-4.3.3\bin\Rscript.exe",
        ]
    } else {
        vec!["/usr/bin/Rscript", "/usr/local/bin/Rscript"]
    };

    for path in &candidates {
        if std::path::Path::new(path).exists() {
            return Some(path.to_string());
        }
    }

    // Try which/where
    let cmd = if cfg!(target_os = "windows") { "where" } else { "which" };
    if let Ok(output) = Command::new(cmd).arg("Rscript").output() {
        if output.status.success() {
            let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !path.is_empty() {
                return Some(path);
            }
        }
    }

    None
}

/// Single R subprocess returning version string + MARMOT install status.
/// Saves one full R startup (~500ms) compared to calling get_r_version + check_marmot_installed sequentially.
pub fn get_r_info(rscript_path: &str) -> Result<(String, bool), String> {
    let r_expr = concat!(
        "cat('MARMOT_R_INFO:',",
        " R.version.string, '|',",
        " tolower(requireNamespace('MARMOT', quietly=TRUE)),",
        " '\\n', sep='')"
    );
    let output = Command::new(rscript_path)
        .args(["-e", r_expr])
        .env("PATH", enrich_path())
        .output()
        .map_err(|e| e.to_string())?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let sentinel = "MARMOT_R_INFO:";
    if let Some(line) = stdout.lines().find(|l| l.starts_with(sentinel)) {
        let rest = &line[sentinel.len()..];
        let parts: Vec<&str> = rest.splitn(2, '|').collect();
        if parts.len() == 2 {
            return Ok((parts[0].trim().to_string(), parts[1].trim() == "true"));
        }
    }
    Ok(("R (version unknown)".to_string(), false))
}

fn rscript_cache_path() -> std::path::PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    std::path::Path::new(&home)
        .join(".config").join("marmot").join("rscript_path.txt")
}

pub fn load_cached_rscript() -> Option<String> {
    let cached = std::fs::read_to_string(rscript_cache_path()).ok()?.trim().to_string();
    if std::path::Path::new(&cached).exists() { Some(cached) } else { None }
}

pub fn save_cached_rscript(path: &str) {
    if let Some(p) = rscript_cache_path().parent() {
        let _ = std::fs::create_dir_all(p);
    }
    let _ = std::fs::write(rscript_cache_path(), path);
}

pub fn spawn_pipeline(
    app: AppHandle,
    rscript_path: String,
    metadata_path: String,
    run_name: String,
    process: SharedProcess,
) {
    std::thread::spawn(move || {
        let safe_metadata = metadata_path
            .replace('\\', "/")
            .replace('"', "\\\"");
        let safe_name = run_name.replace('"', "\\\"");
        let r_expr = format!(
            "MARMOT::marmot(metadata=\"{}\", name=\"{}\", render=TRUE)",
            safe_metadata,
            safe_name
        );

        let mut cmd = Command::new(&rscript_path);
        cmd.args(["-e", &r_expr])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        // Ensure GUI-launched processes get the full PATH (macOS GUI apps
        // don't inherit the user's shell PATH, so tools like Quarto,
        // conda, etc. won't be found otherwise)
        let enriched_path = enrich_path();
        cmd.env("PATH", &enriched_path);

        // On Unix, create new process group for clean kill
        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt;
            cmd.process_group(0);
        }

        let mut child = match cmd.spawn() {
            Ok(c) => c,
            Err(e) => {
                let _ = app.emit("pipeline-log", format!("ERROR: Failed to start Rscript: {}", e));
                let _ = app.emit("pipeline-done", serde_json::json!({"success": false, "error": e.to_string()}));
                return;
            }
        };

        let pid = child.id();
        if let Ok(mut proc) = process.lock() {
            proc.pid = Some(pid);
            proc.running = true;
        }

        let _ = app.emit("pipeline-log", format!("Started pipeline (PID: {})", pid));

        // Stream stdout
        if let Some(stdout) = child.stdout.take() {
            let app_clone = app.clone();
            let reader = BufReader::new(stdout);
            std::thread::spawn(move || {
                for line in reader.lines() {
                    if let Ok(line) = line {
                        let _ = app_clone.emit("pipeline-log", line);
                    }
                }
            });
        }

        // Stream stderr
        if let Some(stderr) = child.stderr.take() {
            let app_clone = app.clone();
            let reader = BufReader::new(stderr);
            std::thread::spawn(move || {
                for line in reader.lines() {
                    if let Ok(line) = line {
                        let _ = app_clone.emit("pipeline-log", line);
                    }
                }
            });
        }

        // Wait for completion
        match child.wait() {
            Ok(status) => {
                let success = status.success();
                let _ = app.emit("pipeline-log",
                    if success { "Pipeline completed successfully.".to_string() }
                    else { format!("Pipeline exited with code: {}", status.code().unwrap_or(-1)) }
                );
                let _ = app.emit("pipeline-done", serde_json::json!({"success": success}));
            }
            Err(e) => {
                let _ = app.emit("pipeline-log", format!("Pipeline error: {}", e));
                let _ = app.emit("pipeline-done", serde_json::json!({"success": false, "error": e.to_string()}));
            }
        }

        if let Ok(mut proc) = process.lock() {
            proc.pid = None;
            proc.running = false;
        }
    });
}

pub fn kill_pipeline(process: &SharedProcess) -> Result<(), String> {
    let proc = process.lock().map_err(|e| format!("Lock error: {}", e))?;
    if let Some(pid) = proc.pid {
        #[cfg(unix)]
        {
            // Kill process group; follow up with SIGKILL after 2s if still alive
            unsafe {
                libc::kill(-(pid as libc::pid_t), libc::SIGTERM);
            }
            let pgid = -(pid as libc::pid_t);
            std::thread::spawn(move || {
                std::thread::sleep(std::time::Duration::from_secs(2));
                unsafe {
                    // SIGKILL if process group is still alive (kill returns 0)
                    if libc::kill(pgid, 0) == 0 {
                        libc::kill(pgid, libc::SIGKILL);
                    }
                }
            });
        }
        #[cfg(windows)]
        {
            let _ = Command::new("taskkill")
                .args(["/T", "/F", "/PID", &pid.to_string()])
                .output();
        }
        Ok(())
    } else {
        Err("No running pipeline to cancel".to_string())
    }
}
