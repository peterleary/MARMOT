use std::collections::HashMap;
use std::fs;
use std::io::{Read, Seek, SeekFrom};

use crate::process::new_command;

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
    {
        new_command("open")
            .arg(&path)
            .spawn()
            .map_err(|e| format!("Failed to open '{}': {}", path, e))?;
    }
    #[cfg(target_os = "linux")]
    {
        new_command("xdg-open")
            .arg(&path)
            .spawn()
            .map_err(|e| format!("Failed to open '{}': {}", path, e))?;
    }
    #[cfg(target_os = "windows")]
    {
        // `explorer <file>` doesn't reliably launch a file's default handler —
        // it opens File Explorer instead. `cmd /c start "" "<path>"` routes
        // through ShellExecute, which opens HTML in the default browser and
        // still handles directories correctly.
        new_command("cmd")
            .args(["/c", "start", "", &path])
            .spawn()
            .map_err(|e| format!("Failed to open '{}': {}", path, e))?;
    }
    Ok(())
}

#[derive(serde::Serialize, Debug, PartialEq)]
pub struct FcsMarker {
    /// $PnN — short/channel name (e.g. "FL1-A", "FSC-A", "Ce140Di")
    pub short_name: String,
    /// $PnS — long/reagent name (e.g. "CD3", "CD4"); empty if not set
    pub long_name: String,
    /// True for scatter / time / event / index channels we'd normally
    /// exclude from the analysis panel.
    pub is_scatter: bool,
}

/// Parse the TEXT segment of an FCS 2.0/3.0/3.1 file and return one entry
/// per parameter ($P1..$PnN/$PnS). Reads only the header + TEXT segment,
/// never loads the DATA segment — cheap even on multi-GB FCS files.
#[tauri::command]
pub fn peek_fcs_markers(path: String) -> Result<Vec<FcsMarker>, String> {
    let mut f = fs::File::open(&path)
        .map_err(|e| format!("Cannot open '{}': {}", path, e))?;

    // FCS header is 58 bytes fixed:
    //   0..6   version ("FCS3.1" / "FCS3.0" / "FCS2.0")
    //   6..10  4 spaces
    //  10..18  TEXT segment start offset (8 ASCII digits, space-padded)
    //  18..26  TEXT segment end offset (inclusive)
    //  26..34  DATA start, 34..42 DATA end, 42..50 ANALYSIS start, 50..58 ANALYSIS end
    let mut header = [0u8; 58];
    f.read_exact(&mut header)
        .map_err(|e| format!("FCS header read failed: {}", e))?;

    let version = std::str::from_utf8(&header[0..6]).unwrap_or("");
    if !version.starts_with("FCS") {
        return Err(format!("'{}' is not an FCS file (version field: {:?})", path, version));
    }

    let parse_off = |bytes: &[u8]| -> Result<usize, String> {
        let s = std::str::from_utf8(bytes)
            .map_err(|e| format!("offset decode: {}", e))?
            .trim();
        s.parse::<usize>()
            .map_err(|e| format!("bad offset {:?}: {}", s, e))
    };
    let text_start = parse_off(&header[10..18])?;
    let text_end = parse_off(&header[18..26])?;
    if text_end < text_start || text_start == 0 {
        return Err("FCS TEXT segment offsets look invalid".into());
    }

    // TEXT segment is typically a few KB. Guard against pathological values
    // (some older FCS writers punt the real offsets into keywords when the
    // 8-digit field is too small — we don't handle that rare case).
    let text_len = text_end - text_start + 1;
    if text_len > 4 * 1024 * 1024 {
        return Err(format!("FCS TEXT segment unexpectedly large ({} bytes)", text_len));
    }

    f.seek(SeekFrom::Start(text_start as u64))
        .map_err(|e| format!("seek to TEXT: {}", e))?;
    let mut text = vec![0u8; text_len];
    f.read_exact(&mut text)
        .map_err(|e| format!("TEXT read: {}", e))?;

    // First byte of the TEXT segment is the keyword/value delimiter.
    // The segment then holds: delim keyword1 delim value1 delim ... delim
    // (We ignore the rare escaped-delimiter case of two delimiters in a row.)
    if text.is_empty() {
        return Err("Empty TEXT segment".into());
    }
    let delim = text[0];
    let parts: Vec<&[u8]> = text[1..].split(|&b| b == delim).filter(|p| !p.is_empty()).collect();

    let mut kv: HashMap<String, String> = HashMap::new();
    let mut i = 0;
    while i + 1 < parts.len() {
        let key = String::from_utf8_lossy(parts[i]).to_ascii_uppercase();
        let val = String::from_utf8_lossy(parts[i + 1]).into_owned();
        kv.insert(key, val);
        i += 2;
    }

    let par: usize = kv
        .get("$PAR")
        .ok_or("FCS TEXT missing required $PAR keyword")?
        .trim()
        .parse()
        .map_err(|e: std::num::ParseIntError| format!("bad $PAR: {}", e))?;

    let mut markers = Vec::with_capacity(par);
    for n in 1..=par {
        let short = kv
            .get(&format!("$P{}N", n))
            .cloned()
            .unwrap_or_default();
        let long = kv
            .get(&format!("$P{}S", n))
            .cloned()
            .unwrap_or_default();
        let is_scatter = is_non_marker_channel(&short, &long);
        markers.push(FcsMarker {
            short_name: short,
            long_name: long,
            is_scatter,
        });
    }
    Ok(markers)
}

/// Decide whether a given FCS channel is a scatter / time / metadata
/// channel (i.e. not an actual marker to include in analysis). Checks
/// BOTH the short name ($PnN) and long name ($PnS) case-insensitively,
/// and covers:
///   - FSC/SSC prefixes and "Forward Scatter" / "Side Scatter" full forms
///   - Time, Event_length, Cell_length
///   - CyTOF metadata: Center, Offset, Width, Residual
///   - Misc: INDEX, EVENT (anything starting with "EVENT")
pub fn is_non_marker_channel(short: &str, long: &str) -> bool {
    let s = short.trim().to_ascii_uppercase();
    let l = long.trim().to_ascii_uppercase();
    // Prefix-based (either field starting with one of these)
    const PREFIXES: &[&str] = &["FSC", "SSC", "TIME", "EVENT", "INDEX"];
    for p in PREFIXES {
        if s.starts_with(p) || l.starts_with(p) {
            return true;
        }
    }
    // Exact (case-insensitive) matches — short and unambiguous
    const EXACT: &[&str] = &[
        "TIME",
        "EVENT_LENGTH",
        "EVENTLENGTH",
        "CELL_LENGTH",
        "CELLLENGTH",
        "CENTER",
        "OFFSET",
        "WIDTH",
        "RESIDUAL",
        "FLOW RATE",
    ];
    for e in EXACT {
        if s == *e || l == *e {
            return true;
        }
    }
    // Substring matches for the verbose scatter-name forms
    const CONTAINS: &[&str] = &["FORWARD SCATTER", "SIDE SCATTER"];
    for c in CONTAINS {
        if s.contains(c) || l.contains(c) {
            return true;
        }
    }
    false
}

/// Natural (human) sort: treat runs of digits as numbers, so "A2" < "A10"
/// and "export_A9 Well_009" < "export_A10 Well_010". Otherwise compares
/// case-insensitively, character by character.
pub fn natural_cmp(a: &str, b: &str) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    let mut ai = a.chars().peekable();
    let mut bi = b.chars().peekable();
    loop {
        match (ai.peek().copied(), bi.peek().copied()) {
            (None, None) => return Ordering::Equal,
            (None, _) => return Ordering::Less,
            (_, None) => return Ordering::Greater,
            (Some(ac), Some(bc)) if ac.is_ascii_digit() && bc.is_ascii_digit() => {
                // Consume digit runs on both sides and compare numerically.
                // Use u128 to avoid overflow on pathologically long numbers.
                let mut a_num: u128 = 0;
                let mut b_num: u128 = 0;
                while let Some(&c) = ai.peek() {
                    if let Some(d) = c.to_digit(10) {
                        a_num = a_num.saturating_mul(10).saturating_add(d as u128);
                        ai.next();
                    } else { break; }
                }
                while let Some(&c) = bi.peek() {
                    if let Some(d) = c.to_digit(10) {
                        b_num = b_num.saturating_mul(10).saturating_add(d as u128);
                        bi.next();
                    } else { break; }
                }
                match a_num.cmp(&b_num) {
                    Ordering::Equal => continue,
                    ord => return ord,
                }
            }
            (Some(ac), Some(bc)) => {
                let al = ac.to_ascii_lowercase();
                let bl = bc.to_ascii_lowercase();
                match al.cmp(&bl) {
                    Ordering::Equal => { ai.next(); bi.next(); }
                    ord => return ord,
                }
            }
        }
    }
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

    fcs_files.sort_by(|a, b| natural_cmp(a, b));
    Ok(fcs_files)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sort(mut v: Vec<&str>) -> Vec<&str> {
        v.sort_by(|a, b| natural_cmp(a, b));
        v
    }

    #[test]
    fn natural_sort_well_plate_names() {
        let input = vec!["A10", "A1", "A2", "B10", "B1", "B2"];
        assert_eq!(sort(input), vec!["A1", "A2", "A10", "B1", "B2", "B10"]);
    }

    #[test]
    fn natural_sort_real_fcs_filenames() {
        let input = vec![
            "export_A1 Well_001_FMT013.fcs",
            "export_A10 Well_010_FMT013.fcs",
            "export_A2 Well_002_FMT013.fcs",
            "export_B1 Well_013_FMT013.fcs",
            "export_B10 Well_022_FMT013.fcs",
            "export_B2 Well_014_FMT013.fcs",
        ];
        assert_eq!(
            sort(input),
            vec![
                "export_A1 Well_001_FMT013.fcs",
                "export_A2 Well_002_FMT013.fcs",
                "export_A10 Well_010_FMT013.fcs",
                "export_B1 Well_013_FMT013.fcs",
                "export_B2 Well_014_FMT013.fcs",
                "export_B10 Well_022_FMT013.fcs",
            ]
        );
    }

    #[test]
    fn natural_sort_case_insensitive() {
        let input = vec!["bar", "Apple", "apple1", "Apple2"];
        // Case-insensitive lex on letters, numeric on digits.
        // "Apple" and "apple1" share prefix "apple"; then "" < "1", so Apple first.
        assert_eq!(sort(input), vec!["Apple", "apple1", "Apple2", "bar"]);
    }

    #[test]
    fn natural_sort_multi_digit_runs() {
        let input = vec!["file_1", "file_100", "file_20", "file_3"];
        assert_eq!(sort(input), vec!["file_1", "file_3", "file_20", "file_100"]);
    }

    #[test]
    fn natural_sort_empty_and_single() {
        assert_eq!(natural_cmp("", ""), std::cmp::Ordering::Equal);
        assert_eq!(natural_cmp("", "a"), std::cmp::Ordering::Less);
        assert_eq!(natural_cmp("a", ""), std::cmp::Ordering::Greater);
    }

    #[test]
    fn non_marker_prefix_match_short_name() {
        assert!(is_non_marker_channel("FSC-A", ""));
        assert!(is_non_marker_channel("FSC-H", ""));
        assert!(is_non_marker_channel("SSC-A", ""));
        assert!(is_non_marker_channel("SSC-B-A", ""));
        assert!(is_non_marker_channel("Time", ""));
    }

    #[test]
    fn non_marker_prefix_match_long_name() {
        // Short name is a generic descriptor; long name is SSC-A
        assert!(is_non_marker_channel("V710-A", "SSC-A"));
        assert!(is_non_marker_channel("B1-A", "FSC-H"));
    }

    #[test]
    fn non_marker_full_word_scatter() {
        assert!(is_non_marker_channel("", "Forward Scatter"));
        assert!(is_non_marker_channel("", "Side Scatter"));
        assert!(is_non_marker_channel("FS Lin", "Forward Scatter Linear"));
    }

    #[test]
    fn non_marker_cytof_metadata() {
        assert!(is_non_marker_channel("Center", ""));
        assert!(is_non_marker_channel("Offset", ""));
        assert!(is_non_marker_channel("Width", ""));
        assert!(is_non_marker_channel("Residual", ""));
        assert!(is_non_marker_channel("Event_length", ""));
        assert!(is_non_marker_channel("Cell_length", ""));
    }

    #[test]
    fn real_markers_are_not_scatter() {
        assert!(!is_non_marker_channel("APC-A", "CD80"));
        assert!(!is_non_marker_channel("BUV395-A", "CD45"));
        assert!(!is_non_marker_channel("Alexa Fluor 532-A", "CD8a"));
        assert!(!is_non_marker_channel("Ce140Di", "CD45"));
        assert!(!is_non_marker_channel("Y89Di", "CD45"));
    }
}
