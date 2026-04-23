use std::process::Stdio;
use std::io::{BufRead, BufReader};
use std::sync::{Arc, Mutex};
use tauri::{AppHandle, Emitter};

use super::new_command;

/// Parse a Quarto chunk-progress line such as `58/249 [dim_reduction]`
/// or `51/249` (for unnamed chunks). Returns `(done, total, chunk_name)`,
/// where `chunk_name` is empty when the chunk is anonymous.
///
/// Quarto prints these to stdout as it starts rendering each code chunk.
/// We use them to drive the GUI progress bar without requiring any changes
/// to the pipeline code.
pub fn parse_quarto_progress(line: &str) -> Option<(u32, u32, String)> {
    let s = line.trim();
    // Expect: DIGITS '/' DIGITS [optional whitespace + '[' NAME ']']
    let slash = s.find('/')?;
    let (a, rest) = s.split_at(slash);
    let done: u32 = a.parse().ok()?;
    let rest = &rest[1..]; // skip '/'
    let total_end = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
    if total_end == 0 { return None; }
    let total: u32 = rest[..total_end].parse().ok()?;
    if total == 0 { return None; }
    let tail = rest[total_end..].trim_start();
    let chunk = if tail.starts_with('[') {
        let close = tail.find(']')?;
        tail[1..close].to_string()
    } else if tail.is_empty() {
        String::new()
    } else {
        // Trailing text that isn't a [name] — not a progress line
        return None;
    };
    Some((done, total, chunk))
}

/// Strip ANSI escape codes (colours, bold, etc.) so log output is readable
/// in the GUI's plain-text log panel.
pub fn strip_ansi(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            // CSI sequence: ESC [ ... final_byte
            if chars.peek() == Some(&'[') {
                chars.next(); // consume '['
                // consume until a letter (the final byte of the CSI sequence)
                while let Some(&ch) = chars.peek() {
                    chars.next();
                    if ch.is_ascii_alphabetic() { break; }
                }
            }
            // else: bare ESC, skip it
        } else {
            out.push(c);
        }
    }
    out
}

/// Cross-platform home directory: HOME on Unix, USERPROFILE on Windows.
pub(crate) fn home_dir() -> String {
    std::env::var("HOME")
        .or_else(|_| std::env::var("USERPROFILE"))
        .unwrap_or_else(|_| ".".to_string())
}

/// Build an enriched PATH that includes common tool locations.
/// macOS GUI apps inherit a minimal PATH (/usr/bin:/bin:/usr/sbin:/sbin),
/// missing Homebrew, Quarto, conda, user-local bins, etc.
pub(crate) fn enrich_path() -> String {
    let current = std::env::var("PATH").unwrap_or_default();
    let home = home_dir();

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
            let _ = new_command("taskkill")
                .args(["/T", "/F", "/PID", &pid.to_string()])
                .output();
        }
    }
}

pub fn find_quarto() -> Option<String> {
    let candidates: Vec<String> = if cfg!(target_os = "macos") {
        vec![
            "/Applications/quarto/bin/quarto".to_string(),
            "/opt/homebrew/bin/quarto".to_string(),
            "/usr/local/bin/quarto".to_string(),
        ]
    } else if cfg!(target_os = "windows") {
        vec![
            r"C:\Program Files\Quarto\bin\quarto.exe".to_string(),
        ]
    } else {
        // Linux: include ~/.local/bin where our auto-installer puts Quarto
        let home = home_dir();
        vec![
            "/usr/bin/quarto".to_string(),
            "/usr/local/bin/quarto".to_string(),
            format!("{}/.local/bin/quarto", home),
        ]
    };

    for path in &candidates {
        if std::path::Path::new(path.as_str()).exists() {
            return Some(path.to_string());
        }
    }

    // Use enriched PATH for the fallback so ~/.local/bin etc. are searched
    let cmd = if cfg!(target_os = "windows") { "where" } else { "which" };
    if let Ok(output) = new_command(cmd)
        .arg("quarto")
        .env("PATH", enrich_path())
        .output()
    {
        if output.status.success() {
            let path = String::from_utf8_lossy(&output.stdout)
                .lines().next().unwrap_or("").trim().to_string();
            if !path.is_empty() {
                return Some(path);
            }
        }
    }

    None
}

pub fn get_quarto_version(quarto_path: &str) -> Result<String, String> {
    let output = new_command(quarto_path)
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
    let home = home_dir();
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
            "/opt/homebrew/bin/Rscript".to_string(),
            "/usr/local/bin/Rscript".to_string(),
            "/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript".to_string(),
        ]
    } else if cfg!(target_os = "windows") {
        // Dynamically scan C:\Program Files\R\ for any R-* directories (newest first)
        scan_windows_r_dirs()
    } else {
        vec![
            "/usr/bin/Rscript".to_string(),
            "/usr/local/bin/Rscript".to_string(),
        ]
    };

    for path in &candidates {
        if std::path::Path::new(path).exists() {
            return Some(path.to_string());
        }
    }

    // Try which/where (use enriched PATH so user-local installs are found)
    let cmd = if cfg!(target_os = "windows") { "where" } else { "which" };
    if let Ok(output) = new_command(cmd)
        .arg("Rscript")
        .env("PATH", enrich_path())
        .output()
    {
        if output.status.success() {
            let path = String::from_utf8_lossy(&output.stdout)
                .lines().next().unwrap_or("").trim().to_string();
            if !path.is_empty() {
                return Some(path);
            }
        }
    }

    None
}

/// Scan `C:\Program Files\R\` for R-* directories, return Rscript.exe paths
/// sorted newest-version-first so the latest R is found first.
fn scan_windows_r_dirs() -> Vec<String> {
    let r_base = std::path::Path::new(r"C:\Program Files\R");
    let mut dirs: Vec<String> = Vec::new();
    if let Ok(entries) = std::fs::read_dir(r_base) {
        for entry in entries.flatten() {
            let name = entry.file_name();
            let name_str = name.to_string_lossy();
            if name_str.starts_with("R-") && entry.path().is_dir() {
                dirs.push(name_str.to_string());
            }
        }
    }
    // Sort descending so newest version comes first (e.g. R-4.5.0 before R-4.4.1)
    dirs.sort_by(|a, b| b.cmp(a));
    dirs.iter()
        .map(|d| format!(r"C:\Program Files\R\{}\bin\Rscript.exe", d))
        .collect()
}

/// Single R subprocess returning version string + MARMOT install status.
/// Saves one full R startup (~500ms) compared to calling get_r_version + check_marmot_installed sequentially.
pub fn get_r_info(rscript_path: &str) -> Result<(String, bool), String> {
    let (version, marmot, _pkgs) = get_r_status(rscript_path)?;
    Ok((version, marmot))
}

/// Combined R status probe — runs a single Rscript invocation that returns
/// (R version, MARMOT installed, {optional package availability}). Used at
/// startup to avoid paying the Rscript boot cost twice. `get_r_info` and
/// the legacy `query_installed_packages` both delegate here.
///
/// Output format (single line on stdout):
///   MARMOT_R_STATUS:<version>|<marmot>|{"Rphenograph":<b>,"PeacoQC":<b>,...}
pub fn get_r_status(rscript_path: &str) -> Result<(String, bool, serde_json::Value), String> {
    let r_expr = r#"
req <- function(pkg) tolower(requireNamespace(pkg, quietly=TRUE))
py <- tryCatch({
  status <- MARMOT::marmot_python_status()
  list(PARC = tolower(status$available), pacmap = tolower(status$available))
}, error = function(e) list(PARC = "false", pacmap = "false"))
pairs <- c(
  paste0('"Rphenograph":', req('Rphenograph')),
  paste0('"PeacoQC":',     req('PeacoQC')),
  paste0('"flowAI":',      req('flowAI')),
  paste0('"PARC":',        py$PARC),
  paste0('"pacmap":',      py$pacmap)
)
cat('MARMOT_R_STATUS:', R.version.string, '|',
    req('MARMOT'), '|{', paste(pairs, collapse=','), '}\n', sep='')
"#;
    let output = new_command(rscript_path)
        .args(["-e", r_expr])
        .env("PATH", enrich_path())
        .output()
        .map_err(|e| e.to_string())?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let sentinel = "MARMOT_R_STATUS:";
    let fallback_pkgs = serde_json::json!({
        "Rphenograph": false, "PeacoQC": false, "flowAI": false,
        "PARC": false, "pacmap": false
    });
    if let Some(line) = stdout.lines().find(|l| l.starts_with(sentinel)) {
        let rest = &line[sentinel.len()..];
        let parts: Vec<&str> = rest.splitn(3, '|').collect();
        if parts.len() == 3 {
            let version = parts[0].trim().to_string();
            let marmot_installed = parts[1].trim() == "true";
            let pkgs = serde_json::from_str(parts[2].trim()).unwrap_or_else(|_| fallback_pkgs.clone());
            return Ok((version, marmot_installed, pkgs));
        }
    }
    Ok(("R (version unknown)".to_string(), false, fallback_pkgs))
}

fn rscript_cache_path() -> std::path::PathBuf {
    let home = home_dir();
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

        let mut cmd = new_command(&rscript_path);
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

        // Stream stdout — hold the JoinHandle so we can wait for it.
        // Also detect Quarto chunk-progress lines and emit a structured
        // `pipeline-progress` event for the GUI's progress bar.
        let stdout_handle = child.stdout.take().map(|stdout| {
            let app_clone = app.clone();
            let reader = BufReader::new(stdout);
            std::thread::spawn(move || {
                for line in reader.lines() {
                    if let Ok(line) = line {
                        let clean = strip_ansi(&line);
                        if let Some((done, total, chunk)) = parse_quarto_progress(&clean) {
                            let _ = app_clone.emit("pipeline-progress", serde_json::json!({
                                "done": done,
                                "total": total,
                                "chunk": chunk,
                            }));
                        }
                        let _ = app_clone.emit("pipeline-log", clean);
                    }
                }
            })
        });

        // Stream stderr — hold the JoinHandle so we can wait for it
        let stderr_handle = child.stderr.take().map(|stderr| {
            let app_clone = app.clone();
            let reader = BufReader::new(stderr);
            std::thread::spawn(move || {
                for line in reader.lines() {
                    if let Ok(line) = line {
                        let _ = app_clone.emit("pipeline-log", strip_ansi(&line));
                    }
                }
            })
        });

        // Wait for completion
        let result = child.wait();

        // Wait for reader threads to finish draining all output BEFORE
        // emitting pipeline-done (otherwise the frontend unregisters the
        // log listener and the tail of the output is lost).
        if let Some(h) = stdout_handle { let _ = h.join(); }
        if let Some(h) = stderr_handle { let _ = h.join(); }

        match result {
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
            let _ = new_command("taskkill")
                .args(["/T", "/F", "/PID", &pid.to_string()])
                .output();
        }
        Ok(())
    } else {
        Err("No running pipeline to cancel".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quarto_progress_named_chunk() {
        assert_eq!(
            parse_quarto_progress("58/249 [dim_reduction]"),
            Some((58, 249, "dim_reduction".to_string()))
        );
    }

    #[test]
    fn quarto_progress_anonymous_chunk() {
        assert_eq!(
            parse_quarto_progress("51/249"),
            Some((51, 249, String::new()))
        );
    }

    #[test]
    fn quarto_progress_leading_and_trailing_whitespace() {
        assert_eq!(
            parse_quarto_progress("  50/249 [parc_clustering]     "),
            Some((50, 249, "parc_clustering".to_string()))
        );
    }

    #[test]
    fn quarto_progress_rejects_non_progress_lines() {
        assert_eq!(parse_quarto_progress("Running PaCMAP"), None);
        assert_eq!(parse_quarto_progress("dim: 5000 2"), None);
        assert_eq!(parse_quarto_progress("time elapsed 16.5 seconds"), None);
        assert_eq!(parse_quarto_progress(""), None);
    }

    #[test]
    fn quarto_progress_rejects_totally_unrelated_slash_text() {
        // e.g. file paths, timestamps
        assert_eq!(parse_quarto_progress("/usr/local/bin"), None);
        assert_eq!(parse_quarto_progress("ratio 1/2 of cells"), None);
    }

    #[test]
    fn quarto_progress_zero_total_rejected() {
        assert_eq!(parse_quarto_progress("0/0"), None);
    }
}
