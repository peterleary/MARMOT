//! Pre-flight metadata validation.
//!
//! Validates a `MetadataFile` (already-loaded from Excel) against the FCS
//! folder on disk *before* the pipeline runs, so common setup mistakes
//! surface as UI failures instead of a 20-minute pipeline crash.
//!
//! Three tiers:
//!   1. Structural — metadata-only, no disk I/O
//!   2. Filesystem — file existence / folder readability
//!   3. FCS content — open every FCS file and diff channel lists
//!
//! Tier 3 reuses `read_fcs_summary` (which reuses `read_fcs_text_segment`
//! alongside `peek_fcs_markers`) so we don't maintain two FCS parsers.

use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::commands::filesystem::{read_fcs_summary, FcsMarker, FcsSummary};
use crate::excel::types::{MetadataFile, SheetData};

#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Status { Pass, Warn, Fail, Skip }

#[derive(Serialize, Debug, Clone)]
pub struct CheckResult {
    pub id: String,
    pub label: String,
    pub tier: u8,
    pub status: Status,
    pub message: String,
    pub details: Vec<String>,
}

impl CheckResult {
    fn pass(id: &str, label: &str, tier: u8, message: impl Into<String>) -> Self {
        Self { id: id.into(), label: label.into(), tier, status: Status::Pass, message: message.into(), details: vec![] }
    }
    fn pass_d(id: &str, label: &str, tier: u8, message: impl Into<String>, details: Vec<String>) -> Self {
        Self { id: id.into(), label: label.into(), tier, status: Status::Pass, message: message.into(), details }
    }
    fn warn(id: &str, label: &str, tier: u8, message: impl Into<String>, details: Vec<String>) -> Self {
        Self { id: id.into(), label: label.into(), tier, status: Status::Warn, message: message.into(), details }
    }
    fn fail(id: &str, label: &str, tier: u8, message: impl Into<String>, details: Vec<String>) -> Self {
        Self { id: id.into(), label: label.into(), tier, status: Status::Fail, message: message.into(), details }
    }
    fn skip(id: &str, label: &str, tier: u8, message: impl Into<String>) -> Self {
        Self { id: id.into(), label: label.into(), tier, status: Status::Skip, message: message.into(), details: vec![] }
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct Summary {
    pub passed: u32,
    pub warnings: u32,
    pub failures: u32,
    pub skipped: u32,
}

#[derive(Serialize, Debug, Clone)]
pub struct PreflightReport {
    pub checks: Vec<CheckResult>,
    pub summary: Summary,
}

impl PreflightReport {
    fn from_checks(checks: Vec<CheckResult>) -> Self {
        let mut summary = Summary { passed: 0, warnings: 0, failures: 0, skipped: 0 };
        for c in &checks {
            match c.status {
                Status::Pass => summary.passed += 1,
                Status::Warn => summary.warnings += 1,
                Status::Fail => summary.failures += 1,
                Status::Skip => summary.skipped += 1,
            }
        }
        Self { checks, summary }
    }
}

// ── Known schema ───────────────────────────────────────────────────────────

const FILE_DATA_REQUIRED: &[&str] = &["file_name", "sample_id", "condition"];

/// Whitelist of recognised Study Data headers. Anything outside this set
/// is flagged as unknown (warn — ignored by the pipeline). Anything inside
/// it must match case-sensitively.
const STUDY_DATA_KNOWN: &[&str] = &[
    "Conditions Order",
    "Cells per condition in UMAPs etc.",
    "Conditions To Test",
    "Markers to include",
    "Marker Type",
    "Cofactors for markers to use",
    "Markers to exclude completely",
    "Marker Pairs",
];

const MARKER_TYPE_ALLOWED: &[&str] = &["type", "state"];
const CLUSTER_METHOD_ALLOWED: &[&str] = &["FlowSOM", "MfastPG", "Mphenograph", "Mparc", "Rphenograph", "PARC"];
const DIM_RED_ALLOWED: &[&str] = &["UMAP", "TSNE", "Mpacmap", "pacmap"];
const MARKERS_ROLE_ALLOWED: &[&str] = &["all", "type", "state"];
const RUN_QC_ALLOWED: &[&str] = &["FlowAI", "PeacoQC", "None"];
const THEME_ALLOWED: &[&str] = &["prism", "classic", "bw", "minimal", "void", "light", "dark"];

// Separator patterns we accept for contrast parsing. The canonical form is
// `" over "` (what the Tauri UI writes), but hand-edited Excels often drift.
const CONTRAST_SEP_WORDS: &[&str] = &["over", "vs.", "vs", "versus", "v.", "v"];

// ── helpers ────────────────────────────────────────────────────────────────

/// Case-sensitive exact column lookup. Returns Some(idx) if any header
/// equals `needle` exactly.
fn col_idx(sheet: &SheetData, needle: &str) -> Option<usize> {
    sheet.headers.iter().position(|h| h == needle)
}

/// Returns trimmed, non-empty values from column `idx`. Row length is not
/// assumed — missing cells in a short row are treated as empty.
fn column_values(sheet: &SheetData, idx: usize) -> Vec<String> {
    sheet.rows.iter()
        .filter_map(|r| r.get(idx).map(|s| s.trim().to_string()))
        .filter(|s| !s.is_empty())
        .collect()
}

fn get_setting<'a>(metadata: &'a MetadataFile, name: &str) -> Option<&'a str> {
    metadata.pipeline_settings.iter()
        .find(|s| s.variable == name)
        .map(|s| s.setting.trim())
        .filter(|s| !s.is_empty())
}

fn truthy(v: &str) -> bool { matches!(v.trim().to_ascii_uppercase().as_str(), "TRUE" | "T" | "1" | "YES") }

fn parse_positive_int(v: &str) -> Option<u64> {
    v.trim().parse::<u64>().ok().filter(|n| *n > 0)
}

fn has_unusual_chars(s: &str) -> bool {
    // Anything outside printable ASCII (letters, digits, common punctuation)
    // is flagged — covers unicode subscripts, superscripts, smart quotes,
    // accented characters that R will factor-encode oddly, etc.
    s.chars().any(|c| !c.is_ascii() || c.is_ascii_control())
}

/// Try to parse a contrast cell as `LHS <sep> RHS`. Accepts any of the
/// CONTRAST_SEP_WORDS (case-insensitively) as the separator, and tolerates
/// any whitespace variation — tabs, doubled spaces, non-breaking spaces
/// and other Unicode whitespace all collapse to a single space before
/// matching. The pipeline does the same normalisation, so whitespace
/// artefacts from hand-edited Excel shouldn't fail — but we do flag them
/// via `canonical` so the user can tidy the cell.
///
/// Returns `Err(reason)` if unparseable; otherwise `Ok((lhs, rhs, canonical))`
/// where `canonical` is `Some("A over B")` iff the original cell was not
/// already in canonical form (bad separator word OR stray whitespace).
fn parse_contrast(cell: &str) -> Result<(String, String, Option<String>), String> {
    // Collapse any run of Unicode whitespace to a single ASCII space, and
    // trim leading/trailing whitespace.
    let normalized: String = cell.split_whitespace().collect::<Vec<_>>().join(" ");
    if normalized.is_empty() {
        return Err(format!("empty contrast cell: {:?}", cell));
    }
    let lc = normalized.to_ascii_lowercase();
    // Earliest match wins; if two separator words start at the same index,
    // the longer one wins (so `vs` / `versus` aren't shadowed by `v`).
    let mut best: Option<(usize, usize, &str)> = None; // (start, end, word)
    for sep in CONTRAST_SEP_WORDS {
        let needle = format!(" {} ", sep);
        if let Some(pos) = lc.find(&needle) {
            let end = pos + needle.len();
            match best {
                None => best = Some((pos, end, sep)),
                Some((bs, _, bw)) if pos < bs || (pos == bs && sep.len() > bw.len()) => {
                    best = Some((pos, end, sep));
                }
                _ => {}
            }
        }
    }
    let Some((start, end, _word)) = best else {
        // Specific detection: if the normalized cell is just a separator
        // word ("over", "vs", etc.), the user left both contrast dropdowns
        // unset and the UI saved " over " with empty sides. Emit a friendlier
        // message so "row 4" doesn't feel cryptic when the dropdowns visually
        // read as "-- over --".
        if CONTRAST_SEP_WORDS.iter().any(|w| lc == *w) {
            return Err(format!(
                "cell {:?} has no conditions on either side — both contrast dropdowns appear unset",
                cell
            ));
        }
        return Err(format!("could not parse cell {:?} (expected 'A over B')", cell));
    };

    let lhs = normalized[..start].trim().to_string();
    let rhs = normalized[end..].trim().to_string();
    if lhs.is_empty() || rhs.is_empty() {
        return Err(format!("empty half: {:?}", cell));
    }

    let canonical_form = format!("{} over {}", lhs, rhs);
    // Flag as non-canonical if the original differs from `"lhs over rhs"`
    // in ANY way — wrong separator word, stray whitespace, tabs, etc.
    // (Case differences in the conditions themselves are handled upstream.)
    let canonical = if cell == canonical_form { None } else { Some(canonical_form) };
    Ok((lhs, rhs, canonical))
}

// ── Tier 1 — structural ────────────────────────────────────────────────────

fn check_file_data_not_empty(metadata: &MetadataFile) -> CheckResult {
    if metadata.file_data.rows.is_empty() {
        CheckResult::fail("file_data_not_empty", "File Data has rows", 1,
            "File Data sheet has no rows — add samples before running.", vec![])
    } else {
        CheckResult::pass("file_data_not_empty", "File Data has rows", 1,
            format!("{} sample rows", metadata.file_data.rows.len()))
    }
}

fn check_required_file_data_columns(metadata: &MetadataFile) -> CheckResult {
    let missing: Vec<String> = FILE_DATA_REQUIRED.iter()
        .filter(|c| col_idx(&metadata.file_data, c).is_none())
        .map(|s| (*s).to_string())
        .collect();
    if missing.is_empty() {
        CheckResult::pass("file_data_columns", "Required File Data columns present", 1,
            "file_name, sample_id, condition (case-sensitive)")
    } else {
        CheckResult::fail("file_data_columns", "Required File Data columns present", 1,
            format!("Missing column(s): {}", missing.join(", ")), missing)
    }
}

fn check_duplicate_filenames(metadata: &MetadataFile) -> CheckResult {
    let Some(idx) = col_idx(&metadata.file_data, "file_name") else {
        return CheckResult::skip("duplicate_filenames", "No duplicate filenames", 1, "file_name column missing");
    };
    let mut seen: HashMap<String, u32> = HashMap::new();
    for v in column_values(&metadata.file_data, idx) {
        *seen.entry(v).or_insert(0) += 1;
    }
    let dups: Vec<String> = seen.into_iter().filter(|(_, n)| *n > 1)
        .map(|(k, n)| format!("{} ({}×)", k, n)).collect();
    if dups.is_empty() {
        CheckResult::pass("duplicate_filenames", "No duplicate filenames", 1, "All file_name values are unique")
    } else {
        CheckResult::fail("duplicate_filenames", "No duplicate filenames", 1,
            format!("{} duplicate filename(s)", dups.len()), dups)
    }
}

fn check_unique_sample_ids(metadata: &MetadataFile) -> CheckResult {
    let Some(idx) = col_idx(&metadata.file_data, "sample_id") else {
        return CheckResult::skip("sample_ids_unique", "Sample IDs unique", 1, "sample_id column missing");
    };
    let mut seen: HashMap<String, u32> = HashMap::new();
    for v in column_values(&metadata.file_data, idx) {
        *seen.entry(v).or_insert(0) += 1;
    }
    let dups: Vec<String> = seen.into_iter().filter(|(_, n)| *n > 1)
        .map(|(k, n)| format!("{} ({}×)", k, n)).collect();
    if dups.is_empty() {
        CheckResult::pass("sample_ids_unique", "Sample IDs unique", 1, "All sample_id values are unique")
    } else {
        CheckResult::fail("sample_ids_unique", "Sample IDs unique", 1,
            format!("{} duplicate sample_id value(s)", dups.len()), dups)
    }
}

fn check_sample_id_chars(metadata: &MetadataFile) -> CheckResult {
    let Some(idx) = col_idx(&metadata.file_data, "sample_id") else {
        return CheckResult::skip("sample_id_chars", "Sample IDs have no unusual characters", 1, "sample_id column missing");
    };
    let flagged: Vec<String> = column_values(&metadata.file_data, idx).into_iter()
        .filter(|v| has_unusual_chars(v))
        .collect();
    if flagged.is_empty() {
        CheckResult::pass("sample_id_chars", "Sample IDs have no unusual characters", 1, "All plain ASCII")
    } else {
        CheckResult::warn("sample_id_chars", "Sample IDs have no unusual characters", 1,
            format!("{} sample_id value(s) contain unusual characters (unicode, control) — may confuse R", flagged.len()),
            flagged)
    }
}

fn check_blank_conditions(metadata: &MetadataFile) -> CheckResult {
    let Some(idx) = col_idx(&metadata.file_data, "condition") else {
        return CheckResult::skip("conditions_not_blank", "Every sample has a condition", 1, "condition column missing");
    };
    let fn_idx = col_idx(&metadata.file_data, "file_name");
    let offenders: Vec<String> = metadata.file_data.rows.iter().enumerate()
        .filter_map(|(i, r)| {
            let v = r.get(idx).map(|s| s.trim()).unwrap_or("");
            if v.is_empty() {
                let label = fn_idx.and_then(|fi| r.get(fi)).map(|s| s.as_str()).unwrap_or("");
                Some(if label.is_empty() { format!("row {}", i + 1) } else { format!("row {} ({})", i + 1, label) })
            } else { None }
        })
        .collect();
    if offenders.is_empty() {
        CheckResult::pass("conditions_not_blank", "Every sample has a condition", 1, "OK")
    } else {
        CheckResult::fail("conditions_not_blank", "Every sample has a condition", 1,
            format!("{} sample(s) have a blank condition", offenders.len()), offenders)
    }
}

fn check_condition_chars(metadata: &MetadataFile) -> CheckResult {
    let Some(idx) = col_idx(&metadata.file_data, "condition") else {
        return CheckResult::skip("condition_chars", "Condition names have no unusual characters", 1,
            "condition column missing");
    };
    let flagged: HashSet<String> = column_values(&metadata.file_data, idx).into_iter()
        .filter(|v| has_unusual_chars(v))
        .collect();
    if flagged.is_empty() {
        CheckResult::pass("condition_chars", "Condition names have no unusual characters", 1, "All plain ASCII")
    } else {
        let mut v: Vec<String> = flagged.into_iter().collect();
        v.sort();
        CheckResult::warn("condition_chars", "Condition names have no unusual characters", 1,
            format!("{} condition name(s) contain unusual characters — no guarantee R handles them cleanly", v.len()), v)
    }
}

fn check_study_data_columns_known(metadata: &MetadataFile) -> Vec<CheckResult> {
    let mut out = vec![];
    let known: HashSet<&str> = STUDY_DATA_KNOWN.iter().copied().collect();
    let known_lc: HashMap<String, &str> = STUDY_DATA_KNOWN.iter()
        .map(|h| (h.to_ascii_lowercase(), *h)).collect();

    let mut typos = vec![];
    let mut extras = vec![];
    for h in &metadata.study_data.headers {
        if known.contains(h.as_str()) { continue; }
        if let Some(canonical) = known_lc.get(&h.to_ascii_lowercase()) {
            typos.push(format!("{:?} → did you mean {:?}? (case-sensitive)", h, canonical));
        } else {
            extras.push(h.clone());
        }
    }
    if !typos.is_empty() {
        out.push(CheckResult::fail("study_data_columns_typo", "No typos in Study Data headers", 1,
            format!("{} column header(s) look like typos", typos.len()), typos));
    } else {
        out.push(CheckResult::pass("study_data_columns_typo", "No typos in Study Data headers", 1, "OK"));
    }
    if !extras.is_empty() {
        out.push(CheckResult::warn("study_data_columns_extra", "Unknown Study Data columns", 1,
            format!("{} extra column(s) will be ignored by the pipeline", extras.len()), extras));
    } else {
        out.push(CheckResult::pass("study_data_columns_extra", "Unknown Study Data columns", 1, "None"));
    }
    out
}

fn check_marker_types(metadata: &MetadataFile) -> CheckResult {
    let Some(mt_idx) = col_idx(&metadata.study_data, "Marker Type") else {
        return CheckResult::skip("marker_types_valid", "Marker Type values valid", 1, "Marker Type column missing");
    };
    let allowed: HashSet<&str> = MARKER_TYPE_ALLOWED.iter().copied().collect();
    let bad: Vec<String> = metadata.study_data.rows.iter().enumerate()
        .filter_map(|(i, r)| {
            let v = r.get(mt_idx).map(|s| s.trim()).unwrap_or("");
            if v.is_empty() || allowed.contains(v) { None }
            else { Some(format!("row {}: {:?}", i + 1, v)) }
        })
        .collect();
    if bad.is_empty() {
        CheckResult::pass("marker_types_valid", "Marker Type values valid", 1,
            "All values are 'type' / 'state' / blank (case-sensitive)")
    } else {
        CheckResult::fail("marker_types_valid", "Marker Type values valid", 1,
            format!("{} invalid Marker Type value(s) — must be 'type' or 'state'", bad.len()), bad)
    }
}

fn check_marker_include_exclude_disjoint(metadata: &MetadataFile) -> CheckResult {
    let Some(inc_idx) = col_idx(&metadata.study_data, "Markers to include") else {
        return CheckResult::skip("markers_disjoint", "Include and exclude lists are disjoint", 1,
            "'Markers to include' column missing");
    };
    let Some(exc_idx) = col_idx(&metadata.study_data, "Markers to exclude completely") else {
        return CheckResult::skip("markers_disjoint", "Include and exclude lists are disjoint", 1,
            "'Markers to exclude completely' column missing");
    };
    let inc: HashSet<String> = column_values(&metadata.study_data, inc_idx).into_iter().collect();
    let exc: HashSet<String> = column_values(&metadata.study_data, exc_idx).into_iter().collect();
    let both: Vec<String> = inc.intersection(&exc).cloned().collect();
    if both.is_empty() {
        CheckResult::pass("markers_disjoint", "Include and exclude lists are disjoint", 1, "OK")
    } else {
        let mut v = both;
        v.sort();
        CheckResult::fail("markers_disjoint", "Include and exclude lists are disjoint", 1,
            format!("{} marker(s) appear in BOTH include and exclude columns", v.len()), v)
    }
}

fn check_marker_duplicates(metadata: &MetadataFile) -> Vec<CheckResult> {
    let mut out = vec![];
    for col in &["Markers to include", "Markers to exclude completely"] {
        let Some(idx) = col_idx(&metadata.study_data, col) else { continue; };
        let mut seen: HashMap<String, u32> = HashMap::new();
        for v in column_values(&metadata.study_data, idx) {
            *seen.entry(v).or_insert(0) += 1;
        }
        let dups: Vec<String> = seen.into_iter().filter(|(_, n)| *n > 1)
            .map(|(k, n)| format!("{} ({}×)", k, n)).collect();
        let id = if *col == "Markers to include" { "marker_dupes_include" } else { "marker_dupes_exclude" };
        let label = format!("No duplicates in '{}'", col);
        if dups.is_empty() {
            out.push(CheckResult::pass(id, &label, 1, "OK"));
        } else {
            out.push(CheckResult::fail(id, &label, 1,
                format!("{} duplicate marker name(s)", dups.len()), dups));
        }
    }
    out
}

fn check_cofactors(metadata: &MetadataFile) -> CheckResult {
    let Some(inc_idx) = col_idx(&metadata.study_data, "Markers to include") else {
        return CheckResult::skip("cofactors_present", "Every include marker has a cofactor", 1,
            "'Markers to include' column missing");
    };
    let Some(cof_idx) = col_idx(&metadata.study_data, "Cofactors for markers to use") else {
        return CheckResult::fail("cofactors_present", "Every include marker has a cofactor", 1,
            "'Cofactors for markers to use' column missing — required", vec![]);
    };

    let mut missing = vec![];
    let mut non_numeric = vec![];
    for (i, row) in metadata.study_data.rows.iter().enumerate() {
        let marker = row.get(inc_idx).map(|s| s.trim()).unwrap_or("");
        if marker.is_empty() { continue; }
        let cof = row.get(cof_idx).map(|s| s.trim()).unwrap_or("");
        if cof.is_empty() {
            missing.push(format!("row {}: {}", i + 1, marker));
        } else if cof.parse::<f64>().is_err() {
            non_numeric.push(format!("row {}: {} = {:?}", i + 1, marker, cof));
        }
    }

    let total_bad = missing.len() + non_numeric.len();
    if total_bad == 0 {
        CheckResult::pass("cofactors_present", "Every include marker has a cofactor", 1, "OK")
    } else {
        let mut details = missing;
        details.extend(non_numeric);
        CheckResult::fail("cofactors_present", "Every include marker has a cofactor", 1,
            format!("{} cofactor issue(s) — every include marker needs a numeric cofactor", total_bad), details)
    }
}

fn check_contrasts(metadata: &MetadataFile) -> CheckResult {
    let Some(idx) = col_idx(&metadata.study_data, "Conditions To Test") else {
        return CheckResult::fail("contrasts_valid", "Contrasts reference known conditions", 1,
            "'Conditions To Test' column missing — pipeline requires at least one contrast", vec![]);
    };
    let cond_idx = match col_idx(&metadata.file_data, "condition") {
        Some(i) => i,
        None => return CheckResult::skip("contrasts_valid", "Contrasts reference known conditions", 1,
            "File Data has no 'condition' column"),
    };
    let file_conditions: HashSet<String> = column_values(&metadata.file_data, cond_idx).into_iter().collect();

    // Iterate rows directly so we can report the spreadsheet row number
    // with every problem (row 1 = first data row below the header). Keep
    // the raw, untrimmed cell content so error messages show the real
    // value the user has in the cell — trimming away leading/trailing
    // whitespace hides important clues (e.g. " over " vs "over").
    let rows_with_contrast: Vec<(usize, String)> = metadata.study_data.rows.iter()
        .enumerate()
        .filter_map(|(i, r)| {
            let raw = r.get(idx)?.clone();
            if raw.trim().is_empty() { None } else { Some((i + 1, raw)) }
        })
        .collect();
    if rows_with_contrast.is_empty() {
        return CheckResult::fail("contrasts_valid", "Contrasts reference known conditions", 1,
            "No contrasts defined — pipeline requires at least one", vec![]);
    }

    let mut fail_details: Vec<String> = vec![];
    let mut warn_details: Vec<String> = vec![];
    let mut seen_pairs: HashMap<(String, String), u32> = HashMap::new();
    let mut valid_count: u32 = 0;

    for (row_num, c) in &rows_with_contrast {
        // Unparseable cells (malformed, empty half, just "over", etc.) are
        // filtered out by the pipeline before DA/DS runs — so they're a
        // warning ("will be ignored"), not a fail. If every row is junk we
        // still fail below via the zero-valid check.
        let (lhs, rhs, canonical) = match parse_contrast(c) {
            Ok(p) => p,
            Err(e) => {
                warn_details.push(format!("row {}: {} — will be ignored by the pipeline", row_num, e));
                continue;
            }
        };

        // Non-canonical formatting (stray whitespace, `vs`/`versus` instead
        // of `over`) is a warning only — the pipeline tolerates it, but we
        // nudge the user toward the canonical form.
        if let Some(canon) = &canonical {
            warn_details.push(format!("row {}: non-canonical {:?} → suggest {:?}", row_num, c, canon));
        }

        if lhs == rhs {
            fail_details.push(format!("row {}: self-contrast {:?} (both sides are {:?})", row_num, c, lhs));
            continue;
        }

        let mut this_has_fail = false;
        for p in [&lhs, &rhs] {
            if !file_conditions.contains(p) {
                if file_conditions.iter().any(|c| c.eq_ignore_ascii_case(p)) {
                    fail_details.push(format!("row {}: case mismatch in {:?} — {:?} does not exactly match any condition", row_num, c, p));
                } else {
                    fail_details.push(format!("row {}: unknown condition {:?} in {:?}", row_num, p, c));
                }
                this_has_fail = true;
            }
        }
        if this_has_fail { continue; }

        *seen_pairs.entry((lhs, rhs)).or_insert(0) += 1;
        valid_count += 1;
    }

    let duplicate_pairs: Vec<String> = seen_pairs.into_iter().filter(|(_, n)| *n > 1)
        .map(|((l, r), n)| format!("duplicate: {:?} over {:?} ({}×)", l, r, n)).collect();
    warn_details.extend(duplicate_pairs);

    // Zero-valid guard: if nothing made it through, fail regardless of
    // whether individual rows were marked fail or warn — the pipeline
    // requires at least one runnable contrast.
    if valid_count == 0 && fail_details.is_empty() {
        fail_details.push("No contrast survives parsing — the pipeline will have nothing to test".into());
    }

    if !fail_details.is_empty() {
        let mut combined = fail_details;
        combined.extend(warn_details);
        CheckResult::fail("contrasts_valid", "Contrasts reference known conditions", 1,
            format!("{} problem(s) in contrasts", combined.len()), combined)
    } else if !warn_details.is_empty() {
        CheckResult::warn("contrasts_valid", "Contrasts reference known conditions", 1,
            format!("{} contrast(s) need tidying ({} valid contrast(s) will run)", warn_details.len(), valid_count),
            warn_details)
    } else {
        CheckResult::pass("contrasts_valid", "Contrasts reference known conditions", 1,
            format!("{} contrast(s) OK", rows_with_contrast.len()))
    }
}

fn check_conditions_order(metadata: &MetadataFile) -> CheckResult {
    let Some(idx) = col_idx(&metadata.study_data, "Conditions Order") else {
        return CheckResult::skip("conditions_order", "Conditions Order matches File Data", 1,
            "'Conditions Order' column missing");
    };
    let Some(cond_idx) = col_idx(&metadata.file_data, "condition") else {
        return CheckResult::skip("conditions_order", "Conditions Order matches File Data", 1,
            "File Data 'condition' column missing");
    };
    let listed: HashSet<String> = column_values(&metadata.study_data, idx).into_iter().collect();
    if listed.is_empty() {
        return CheckResult::skip("conditions_order", "Conditions Order matches File Data", 1,
            "No conditions listed in Conditions Order");
    }
    let actual: HashSet<String> = column_values(&metadata.file_data, cond_idx).into_iter().collect();

    let missing: Vec<String> = actual.difference(&listed).cloned().collect();
    let extra: Vec<String> = listed.difference(&actual).cloned().collect();

    if missing.is_empty() && extra.is_empty() {
        CheckResult::pass("conditions_order", "Conditions Order matches File Data", 1,
            format!("{} condition(s) ordered", listed.len()))
    } else {
        let mut details = vec![];
        for m in &missing { details.push(format!("not in Conditions Order: {}", m)); }
        for e in &extra { details.push(format!("unknown in Conditions Order: {}", e)); }
        CheckResult::fail("conditions_order", "Conditions Order matches File Data", 1,
            format!("{} condition(s) missing, {} extra", missing.len(), extra.len()), details)
    }
}

fn check_knn_in_k_values(metadata: &MetadataFile) -> CheckResult {
    let knn = get_setting(metadata, "knn");
    let k_values = get_setting(metadata, "kValuesIWant");
    let (Some(knn), Some(k_values)) = (knn, k_values) else {
        return CheckResult::skip("knn_in_k_values", "knn is one of kValuesIWant", 1,
            "knn or kValuesIWant missing");
    };
    let values: Vec<&str> = k_values.split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty()).collect();
    if values.iter().any(|v| *v == knn) {
        CheckResult::pass("knn_in_k_values", "knn is one of kValuesIWant", 1,
            format!("knn={} ∈ {{{}}}", knn, values.join(", ")))
    } else {
        CheckResult::fail("knn_in_k_values", "knn is one of kValuesIWant", 1,
            format!("knn ({}) is not one of kValuesIWant ({})", knn, k_values), vec![])
    }
}

fn check_k_values_sanity(metadata: &MetadataFile) -> Vec<CheckResult> {
    let mut out = vec![];
    let Some(raw) = get_setting(metadata, "kValuesIWant") else {
        out.push(CheckResult::skip("k_values_sanity", "kValuesIWant looks reasonable", 1, "Not set"));
        return out;
    };
    let parsed: Vec<i64> = raw.split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty())
        .filter_map(|s| s.parse::<i64>().ok())
        .collect();
    if parsed.is_empty() {
        out.push(CheckResult::fail("k_values_sanity", "kValuesIWant looks reasonable", 1,
            format!("kValuesIWant ({:?}) has no valid integers", raw), vec![]));
        return out;
    }

    // Duplicates
    let mut seen: HashMap<i64, u32> = HashMap::new();
    for v in &parsed { *seen.entry(*v).or_insert(0) += 1; }
    let dup_vals: Vec<String> = seen.iter().filter(|(_, n)| **n > 1).map(|(v, _)| v.to_string()).collect();
    if !dup_vals.is_empty() {
        out.push(CheckResult::warn("k_values_dupes", "kValuesIWant has no duplicates", 1,
            format!("Duplicate k value(s): {}", dup_vals.join(", ")), dup_vals));
    } else {
        out.push(CheckResult::pass("k_values_dupes", "kValuesIWant has no duplicates", 1, "OK"));
    }

    // Range
    let out_of_range: Vec<String> = parsed.iter().filter(|v| **v < 10 || **v > 100)
        .map(|v| v.to_string()).collect();
    if !out_of_range.is_empty() {
        out.push(CheckResult::warn("k_values_range", "kValuesIWant in 10–100 range", 1,
            format!("{} value(s) outside 10–100", out_of_range.len()), out_of_range));
    } else {
        out.push(CheckResult::pass("k_values_range", "kValuesIWant in 10–100 range", 1, "OK"));
    }

    // Parity: FlowSOM = even, Rphenograph/MfastPG/PARC = odd
    if let Some(method) = get_setting(metadata, "clusteringMethodToUse") {
        let needs_even = method == "FlowSOM";
        let needs_odd = matches!(method, "Rphenograph" | "MfastPG" | "PARC" | "Mphenograph" | "Mparc");
        if needs_even {
            let bad: Vec<String> = parsed.iter().filter(|v| **v % 2 != 0).map(|v| v.to_string()).collect();
            if !bad.is_empty() {
                out.push(CheckResult::warn("k_values_parity", "kValuesIWant parity matches method", 1,
                    format!("{} uses even k values — got odd: {}", method, bad.join(", ")), bad));
            }
        } else if needs_odd {
            let bad: Vec<String> = parsed.iter().filter(|v| **v % 2 == 0).map(|v| v.to_string()).collect();
            if !bad.is_empty() {
                out.push(CheckResult::warn("k_values_parity", "kValuesIWant parity matches method", 1,
                    format!("{} uses odd k values — got even: {}", method, bad.join(", ")), bad));
            }
        }
    }

    out
}

fn check_enum(metadata: &MetadataFile, var: &str, allowed: &[&str], label: &str) -> CheckResult {
    let id = format!("enum_{}", var);
    let Some(val) = get_setting(metadata, var) else {
        return CheckResult::skip(&id, label, 1, format!("{} not set", var));
    };
    if allowed.iter().any(|a| *a == val) {
        CheckResult::pass(&id, label, 1, format!("{} = {:?}", var, val))
    } else {
        CheckResult::fail(&id, label, 1,
            format!("{} = {:?}; must be one of {}", var, val, allowed.join(", ")), vec![])
    }
}

fn check_numeric(metadata: &MetadataFile, var: &str, allow_blank: bool, label: &str) -> CheckResult {
    let id = format!("numeric_{}", var);
    let raw = metadata.pipeline_settings.iter().find(|s| s.variable == var).map(|s| s.setting.trim());
    match raw {
        None | Some("") => {
            if allow_blank {
                CheckResult::pass(&id, label, 1, format!("{} blank (OK)", var))
            } else {
                CheckResult::fail(&id, label, 1, format!("{} required", var), vec![])
            }
        }
        Some(v) => match parse_positive_int(v) {
            Some(_) => CheckResult::pass(&id, label, 1, format!("{} = {}", var, v)),
            None => CheckResult::fail(&id, label, 1,
                format!("{} = {:?}; must be a positive integer", var, v), vec![]),
        }
    }
}

fn check_coherence_qc(metadata: &MetadataFile) -> CheckResult {
    let use_qc = get_setting(metadata, "useQC").map(truthy).unwrap_or(false);
    let run_qc = get_setting(metadata, "runQC").unwrap_or("");
    if use_qc && run_qc.eq_ignore_ascii_case("None") {
        CheckResult::fail("coherence_qc", "useQC coherent with runQC", 1,
            "useQC=TRUE but runQC=None — can't apply a QC filter you never ran", vec![])
    } else {
        CheckResult::pass("coherence_qc", "useQC coherent with runQC", 1, "OK")
    }
}

fn check_coherence_parallel(metadata: &MetadataFile) -> CheckResult {
    let in_parallel = get_setting(metadata, "runInParallel").map(truthy).unwrap_or(false);
    let n_cores = get_setting(metadata, "nCores").and_then(parse_positive_int).unwrap_or(0);
    if in_parallel && n_cores <= 1 {
        CheckResult::warn("coherence_parallel", "Parallel settings coherent", 1,
            "runInParallel=TRUE but nCores ≤ 1 — no parallel benefit", vec![])
    } else {
        CheckResult::pass("coherence_parallel", "Parallel settings coherent", 1, "OK")
    }
}

fn check_markers_role_has_matches(metadata: &MetadataFile, var: &str, label: &str) -> CheckResult {
    let id = format!("role_{}", var);
    let Some(role) = get_setting(metadata, var) else {
        return CheckResult::skip(&id, label, 1, format!("{} not set", var));
    };
    if role == "all" {
        return CheckResult::pass(&id, label, 1, format!("{} = all", var));
    }
    // For "type" or "state", the Panel must contain at least one marker
    // whose Marker Type matches.
    let Some(mt_idx) = col_idx(&metadata.study_data, "Marker Type") else {
        return CheckResult::fail(&id, label, 1,
            format!("{}={} but 'Marker Type' column missing", var, role), vec![]);
    };
    let any = metadata.study_data.rows.iter()
        .any(|r| r.get(mt_idx).map(|s| s.trim()) == Some(role));
    if any {
        CheckResult::pass(&id, label, 1, format!("{} = {} ✓", var, role))
    } else {
        CheckResult::fail(&id, label, 1,
            format!("{}={} but no marker has Marker Type='{}'", var, role, role), vec![])
    }
}

fn check_reload_folder(metadata: &MetadataFile) -> CheckResult {
    let Some(p) = get_setting(metadata, "RDataFolder") else {
        return CheckResult::pass("reload_folder", "Reload folder", 1, "Not set (fresh run)");
    };
    let path = Path::new(p);
    if !path.exists() || !path.is_dir() {
        CheckResult::fail("reload_folder", "Reload folder", 1,
            format!("RDataFolder does not exist: {}", p), vec![])
    } else {
        CheckResult::pass("reload_folder", "Reload folder", 1, p.to_string())
    }
}

// ── Tier 2 — filesystem ────────────────────────────────────────────────────

fn check_fcs_folder_readable(fcs_dir: &Path) -> CheckResult {
    if !fcs_dir.exists() {
        return CheckResult::fail("fcs_folder_exists", "FCS folder exists", 2,
            format!("Folder does not exist: {}", fcs_dir.display()), vec![]);
    }
    if !fcs_dir.is_dir() {
        return CheckResult::fail("fcs_folder_exists", "FCS folder exists", 2,
            format!("Path is not a directory: {}", fcs_dir.display()), vec![]);
    }
    match std::fs::read_dir(fcs_dir) {
        Ok(_) => CheckResult::pass("fcs_folder_exists", "FCS folder exists", 2, fcs_dir.display().to_string()),
        Err(e) => CheckResult::fail("fcs_folder_exists", "FCS folder exists", 2,
            format!("Cannot read folder: {}", e), vec![]),
    }
}

fn check_files_exist_and_nonempty(metadata: &MetadataFile, fcs_dir: &Path) -> CheckResult {
    let Some(idx) = col_idx(&metadata.file_data, "file_name") else {
        return CheckResult::skip("files_exist", "All listed FCS files exist and are non-empty", 2,
            "file_name column missing");
    };
    let expected = column_values(&metadata.file_data, idx);
    if expected.is_empty() {
        return CheckResult::skip("files_exist", "All listed FCS files exist and are non-empty", 2,
            "No filenames to check");
    }

    let mut missing = vec![];
    let mut empty = vec![];
    for f in &expected {
        let p = fcs_dir.join(f);
        if !p.exists() {
            missing.push(format!("missing: {}", f));
            continue;
        }
        if let Ok(m) = std::fs::metadata(&p) {
            if m.len() == 0 {
                empty.push(format!("0 bytes: {}", f));
            }
        }
    }
    let bad = missing.len() + empty.len();
    if bad == 0 {
        CheckResult::pass("files_exist", "All listed FCS files exist and are non-empty", 2,
            format!("{} files OK", expected.len()))
    } else {
        let mut details = missing;
        details.extend(empty);
        CheckResult::fail("files_exist", "All listed FCS files exist and are non-empty", 2,
            format!("{} of {} files missing or empty", bad, expected.len()), details)
    }
}

fn check_extra_fcs_files(metadata: &MetadataFile, fcs_dir: &Path) -> CheckResult {
    let Some(idx) = col_idx(&metadata.file_data, "file_name") else {
        return CheckResult::skip("extra_fcs_files", "No unreferenced FCS files in folder", 2,
            "file_name column missing");
    };
    let listed: HashSet<String> = column_values(&metadata.file_data, idx).into_iter().collect();
    let Ok(entries) = std::fs::read_dir(fcs_dir) else {
        return CheckResult::skip("extra_fcs_files", "No unreferenced FCS files in folder", 2,
            "Could not enumerate folder");
    };
    let on_disk: Vec<String> = entries.filter_map(|e| e.ok())
        .map(|e| e.file_name().to_string_lossy().to_string())
        .filter(|n| n.to_ascii_lowercase().ends_with(".fcs"))
        .collect();
    let extras: Vec<String> = on_disk.into_iter().filter(|f| !listed.contains(f)).collect();
    if extras.is_empty() {
        CheckResult::pass("extra_fcs_files", "No unreferenced FCS files in folder", 2,
            "Every .fcs file on disk is listed in File Data")
    } else {
        CheckResult::warn("extra_fcs_files", "No unreferenced FCS files in folder", 2,
            format!("{} .fcs file(s) on disk not listed in File Data — may be intentional", extras.len()), extras)
    }
}

fn check_output_dir_writable(fcs_dir: &Path) -> CheckResult {
    match std::fs::metadata(fcs_dir) {
        Ok(m) if m.permissions().readonly() => CheckResult::fail(
            "output_writable", "FCS folder is writable", 2,
            "FCS folder is read-only — pipeline can't write Results_Files_*", vec![]),
        Ok(_) => {
            let probe = fcs_dir.join(".marmot_preflight_probe");
            match std::fs::write(&probe, b"") {
                Ok(_) => { let _ = std::fs::remove_file(&probe);
                    CheckResult::pass("output_writable", "FCS folder is writable", 2, "OK") }
                Err(e) => CheckResult::fail("output_writable", "FCS folder is writable", 2,
                    format!("Cannot write to folder: {}", e), vec![]),
            }
        }
        Err(e) => CheckResult::fail("output_writable", "FCS folder is writable", 2,
            format!("Cannot stat folder: {}", e), vec![]),
    }
}

// ── Tier 3 — FCS content across all files ──────────────────────────────────

fn channel_names(markers: &[FcsMarker]) -> Vec<String> {
    markers.iter()
        .map(|m| if !m.long_name.trim().is_empty() { m.long_name.trim().to_string() }
                 else { m.short_name.trim().to_string() })
        .filter(|s| !s.is_empty())
        .collect()
}

struct ParsedFile {
    name: String,
    summary: FcsSummary,
    channels: Vec<String>,
}

fn format_cells(n: u64) -> String {
    let n = n as f64;
    if n < 1_000.0 { format!("{:.0}", n) }
    else if n < 1_000_000.0 { format!("{:.1} K", n / 1_000.0) }
    else if n < 1_000_000_000.0 { format!("{:.2} M", n / 1_000_000.0) }
    else { format!("{:.2} B", n / 1_000_000_000.0) }
}

fn check_fcs_content(metadata: &MetadataFile, fcs_dir: &Path) -> Vec<CheckResult> {
    let mut out = vec![];

    let Some(fn_idx) = col_idx(&metadata.file_data, "file_name") else {
        out.push(CheckResult::skip("fcs_parseable", "All FCS files parse", 3, "file_name column missing"));
        return out;
    };
    let filenames: Vec<String> = column_values(&metadata.file_data, fn_idx);
    if filenames.is_empty() {
        out.push(CheckResult::skip("fcs_parseable", "All FCS files parse", 3, "No filenames listed"));
        return out;
    }

    // Parse every file that exists. Tier 2 already flagged missing/empty
    // files, so we skip those here rather than double-reporting.
    let mut parsed: Vec<ParsedFile> = vec![];
    let mut parse_errors: Vec<String> = vec![];
    for f in &filenames {
        let p = fcs_dir.join(f);
        if !p.exists() { continue; }
        if let Ok(m) = std::fs::metadata(&p) { if m.len() == 0 { continue; } }
        match read_fcs_summary(p.to_string_lossy().as_ref()) {
            Ok(summary) => {
                let channels = channel_names(&summary.markers);
                parsed.push(ParsedFile { name: f.clone(), summary, channels });
            }
            Err(e) => parse_errors.push(format!("{}: {}", f, e)),
        }
    }

    if !parse_errors.is_empty() {
        out.push(CheckResult::fail("fcs_parseable", "All FCS files parse", 3,
            format!("{} file(s) failed to parse", parse_errors.len()), parse_errors));
    } else {
        out.push(CheckResult::pass("fcs_parseable", "All FCS files parse", 3,
            format!("{} file(s) parsed", parsed.len())));
    }

    if parsed.is_empty() { return out; }

    // Panel consistency — every file must expose the same sorted channel
    // list. Group files by their (sorted) channel fingerprint.
    let mut groups: HashMap<Vec<String>, Vec<String>> = HashMap::new();
    for p in &parsed {
        let mut sig = p.channels.clone();
        sig.sort();
        groups.entry(sig).or_default().push(p.name.clone());
    }
    if groups.len() <= 1 {
        out.push(CheckResult::pass("panel_consistent", "All FCS files have identical channels", 3,
            format!("All {} file(s) share the same panel", parsed.len())));
    } else {
        let mut details: Vec<String> = vec![];
        let mut sorted_groups: Vec<(Vec<String>, Vec<String>)> = groups.into_iter().collect();
        sorted_groups.sort_by_key(|g| std::cmp::Reverse(g.1.len()));
        for (i, (sig, files)) in sorted_groups.iter().enumerate() {
            details.push(format!("— Panel variant {} ({} file(s), {} channels) —", i + 1, files.len(), sig.len()));
            for f in files { details.push(format!("  {}", f)); }
        }
        out.push(CheckResult::fail("panel_consistent", "All FCS files have identical channels", 3,
            format!("{} distinct panel variants across {} file(s)", sorted_groups.len(), parsed.len()), details));
    }

    // Build the union of every channel seen across files so the
    // marker-match check works even if panels diverged (we still want to
    // know which markers were truly unreachable).
    let all_channels: HashSet<String> = parsed.iter().flat_map(|p| p.channels.iter().cloned()).collect();

    // Validate markers in include, exclude, cluster-by (we only have
    // include + exclude today — cluster-by is a pipeline-setting role, not
    // a column of literal names), and the tokens in Marker Pairs.
    let mut marker_details: Vec<String> = vec![];

    for col in &["Markers to include", "Markers to exclude completely"] {
        if let Some(idx) = col_idx(&metadata.study_data, col) {
            for name in column_values(&metadata.study_data, idx) {
                if !all_channels.contains(&name) {
                    marker_details.push(format!("{}: {:?} not in any FCS file", col, name));
                }
            }
        }
    }

    if let Some(idx) = col_idx(&metadata.study_data, "Marker Pairs") {
        for cell in column_values(&metadata.study_data, idx) {
            // Expected form: "CellType: marker1 marker2"
            let after_colon = cell.split_once(':').map(|(_, rhs)| rhs.trim()).unwrap_or(&cell);
            for tok in after_colon.split_whitespace() {
                if tok.is_empty() { continue; }
                if !all_channels.contains(tok) {
                    marker_details.push(format!("Marker Pairs: {:?} in {:?} not in any FCS file", tok, cell));
                }
            }
        }
    }

    if marker_details.is_empty() {
        out.push(CheckResult::pass("markers_match_fcs", "All listed markers exist in FCS files", 3, "OK"));
    } else {
        out.push(CheckResult::fail("markers_match_fcs", "All listed markers exist in FCS files", 3,
            format!("{} marker reference(s) have no match", marker_details.len()), marker_details));
    }

    // Channel coverage — every channel in every FCS must be in include OR
    // exclude. Pipeline crashes otherwise.
    let inc: HashSet<String> = col_idx(&metadata.study_data, "Markers to include")
        .map(|i| column_values(&metadata.study_data, i).into_iter().collect())
        .unwrap_or_default();
    let exc: HashSet<String> = col_idx(&metadata.study_data, "Markers to exclude completely")
        .map(|i| column_values(&metadata.study_data, i).into_iter().collect())
        .unwrap_or_default();
    let accounted: HashSet<String> = inc.union(&exc).cloned().collect();
    let orphans: Vec<String> = all_channels.iter().filter(|c| !accounted.contains(*c)).cloned().collect();
    let mut orphans = orphans;
    orphans.sort();
    if orphans.is_empty() {
        out.push(CheckResult::pass("channel_coverage", "Every FCS channel is listed in the Panel", 3,
            format!("{} channels fully accounted for ({} include, {} exclude)",
                all_channels.len(), inc.len(), exc.len())));
    } else {
        out.push(CheckResult::fail("channel_coverage", "Every FCS channel is listed in the Panel", 3,
            format!("{} FCS channel(s) not in include or exclude — pipeline will crash", orphans.len()), orphans));
    }

    // Cell count + RAM heat index.
    let total_cells: u64 = parsed.iter().map(|p| p.summary.n_cells).sum();
    let n_params = parsed.first().map(|p| p.summary.n_params).unwrap_or(0);
    let predicted_ram_gb = (total_cells as f64) * (n_params as f64) * 8.0 * 2.5 / 1_073_741_824.0;

    // User-defined thresholds:
    //   < 1M       : fine
    //   1M – 5M    : warm (info)
    //   5M – 10M   : hot (warn)
    //   > 10M      : very hot (warn, stronger wording)
    let per_file: Vec<String> = parsed.iter()
        .map(|p| format!("{}: {} cells", p.name, format_cells(p.summary.n_cells)))
        .collect();
    let msg = format!("{} cells across {} files × {} params → ~{:.1} GB peak RAM (2.5× multiplier)",
        format_cells(total_cells), parsed.len(), n_params, predicted_ram_gb);

    let check = if total_cells < 1_000_000 {
        CheckResult::pass_d("cell_count", "Total cells & RAM estimate", 3, msg, per_file)
    } else if total_cells < 5_000_000 {
        CheckResult::pass_d("cell_count", "Total cells & RAM estimate", 3,
            format!("Warm — {}", msg), per_file)
    } else if total_cells < 10_000_000 {
        CheckResult::warn("cell_count", "Total cells & RAM estimate", 3,
            format!("Hot — {} — expect significant memory pressure", msg), per_file)
    } else {
        CheckResult::warn("cell_count", "Total cells & RAM estimate", 3,
            format!("Very hot — {} — you'll need a lot of RAM or downsample", msg), per_file)
    };
    out.push(check);

    out
}

// ── entry point ────────────────────────────────────────────────────────────

fn is_reload_mode(metadata: &MetadataFile) -> bool {
    get_setting(metadata, "RDataFolder").is_some()
}

#[tauri::command]
pub fn check_metadata(metadata: MetadataFile, fcs_folder: String) -> Result<PreflightReport, String> {
    let fcs_dir = Path::new(&fcs_folder);
    let mut checks = vec![];
    let reload = is_reload_mode(&metadata);

    // In reload mode the pipeline re-uses a previously-saved SingleCellExperiment
    // and framesList, so most metadata checks are moot. We do the bare minimum:
    // reload folder exists, FCS folder exists, files exist (the reload path
    // still references FCS filenames). Everything else is skipped.
    if reload {
        checks.push(check_reload_folder(&metadata));
        checks.push(check_file_data_not_empty(&metadata));
        checks.push(check_required_file_data_columns(&metadata));
        if fcs_folder.trim().is_empty() {
            checks.push(CheckResult::fail("fcs_folder_exists", "FCS folder exists", 2,
                "No FCS folder selected", vec![]));
        } else {
            let folder_check = check_fcs_folder_readable(fcs_dir);
            let folder_ok = folder_check.status == Status::Pass;
            checks.push(folder_check);
            if folder_ok {
                checks.push(check_files_exist_and_nonempty(&metadata, fcs_dir));
            }
        }
        return Ok(PreflightReport::from_checks(checks));
    }

    // Tier 1 — structural
    checks.push(check_file_data_not_empty(&metadata));
    checks.push(check_required_file_data_columns(&metadata));
    checks.push(check_duplicate_filenames(&metadata));
    checks.push(check_unique_sample_ids(&metadata));
    checks.push(check_sample_id_chars(&metadata));
    checks.push(check_blank_conditions(&metadata));
    checks.push(check_condition_chars(&metadata));
    checks.extend(check_study_data_columns_known(&metadata));
    checks.push(check_marker_types(&metadata));
    checks.push(check_marker_include_exclude_disjoint(&metadata));
    checks.extend(check_marker_duplicates(&metadata));
    checks.push(check_cofactors(&metadata));
    checks.push(check_contrasts(&metadata));
    checks.push(check_conditions_order(&metadata));
    checks.push(check_knn_in_k_values(&metadata));
    checks.extend(check_k_values_sanity(&metadata));

    // Hardcoded dropdowns
    checks.push(check_enum(&metadata, "clusteringMethodToUse", CLUSTER_METHOD_ALLOWED, "Clustering method valid"));
    checks.push(check_enum(&metadata, "dimRedMethodToUse", DIM_RED_ALLOWED, "Dim. reduction method valid"));
    checks.push(check_enum(&metadata, "runQC", RUN_QC_ALLOWED, "QC method valid"));
    checks.push(check_enum(&metadata, "markersToClusterBy", MARKERS_ROLE_ALLOWED, "markersToClusterBy valid"));
    checks.push(check_enum(&metadata, "markersToDimRedBy", MARKERS_ROLE_ALLOWED, "markersToDimRedBy valid"));
    checks.push(check_enum(&metadata, "themeToUse", THEME_ALLOWED, "Theme valid"));

    // Numeric sanity
    checks.push(check_numeric(&metadata, "nCores", false, "nCores is a positive integer"));
    checks.push(check_numeric(&metadata, "ramPerCore", false, "ramPerCore is a positive integer"));
    checks.push(check_numeric(&metadata, "downsampleTo", true, "downsampleTo is a positive integer or blank"));

    // Cross-field coherence
    checks.push(check_coherence_qc(&metadata));
    checks.push(check_coherence_parallel(&metadata));
    checks.push(check_markers_role_has_matches(&metadata, "markersToClusterBy", "markersToClusterBy has matching markers"));
    checks.push(check_markers_role_has_matches(&metadata, "markersToDimRedBy", "markersToDimRedBy has matching markers"));
    checks.push(check_reload_folder(&metadata));

    // Tier 2 — filesystem
    if fcs_folder.trim().is_empty() {
        checks.push(CheckResult::fail("fcs_folder_exists", "FCS folder exists", 2,
            "No FCS folder selected", vec![]));
    } else {
        let folder_check = check_fcs_folder_readable(fcs_dir);
        let folder_ok = folder_check.status == Status::Pass;
        checks.push(folder_check);
        if folder_ok {
            checks.push(check_files_exist_and_nonempty(&metadata, fcs_dir));
            checks.push(check_extra_fcs_files(&metadata, fcs_dir));
            checks.push(check_output_dir_writable(fcs_dir));
            // Tier 3
            checks.extend(check_fcs_content(&metadata, fcs_dir));
        }
    }

    Ok(PreflightReport::from_checks(checks))
}

// ── tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::excel::types::{MetadataFile, PipelineSetting, SheetData};

    fn mk_setting(var: &str, setting: &str) -> PipelineSetting {
        PipelineSetting { variable: var.into(), setting: setting.into(), info: "".into() }
    }

    fn base_metadata() -> MetadataFile {
        MetadataFile {
            path: None,
            pipeline_settings: vec![
                mk_setting("knn", "20"),
                mk_setting("kValuesIWant", "20, 40"),
                mk_setting("clusteringMethodToUse", "FlowSOM"),
                mk_setting("dimRedMethodToUse", "UMAP"),
                mk_setting("runQC", "PeacoQC"),
                mk_setting("useQC", "FALSE"),
                mk_setting("markersToClusterBy", "all"),
                mk_setting("markersToDimRedBy", "all"),
                mk_setting("themeToUse", "prism"),
                mk_setting("nCores", "4"),
                mk_setting("ramPerCore", "6"),
                mk_setting("runInParallel", "TRUE"),
            ],
            study_data: SheetData {
                headers: vec![
                    "Conditions Order".into(),
                    "Conditions To Test".into(),
                    "Markers to include".into(),
                    "Marker Type".into(),
                    "Cofactors for markers to use".into(),
                    "Markers to exclude completely".into(),
                    "Marker Pairs".into(),
                ],
                rows: vec![
                    vec!["A".into(), "A over B".into(), "CD3".into(),  "type".into(),  "5".into(), "FSC-A".into(), "".into()],
                    vec!["B".into(), "".into(),        "CD4".into(),  "state".into(), "5".into(), "".into(),      "".into()],
                ],
            },
            file_data: SheetData {
                headers: vec!["file_name".into(), "sample_id".into(), "condition".into()],
                rows: vec![
                    vec!["sample1.fcs".into(), "s1".into(), "A".into()],
                    vec!["sample2.fcs".into(), "s2".into(), "B".into()],
                ],
            },
            options: SheetData::new(),
        }
    }

    #[test]
    fn knn_in_k_values_passes_on_match() {
        assert_eq!(check_knn_in_k_values(&base_metadata()).status, Status::Pass);
    }

    #[test]
    fn knn_in_k_values_fails_on_mismatch() {
        let mut m = base_metadata();
        m.pipeline_settings.iter_mut().find(|s| s.variable == "knn").unwrap().setting = "30".into();
        assert_eq!(check_knn_in_k_values(&m).status, Status::Fail);
    }

    #[test]
    fn duplicate_filenames_caught() {
        let mut m = base_metadata();
        m.file_data.rows.push(vec!["sample1.fcs".into(), "s3".into(), "A".into()]);
        let r = check_duplicate_filenames(&m);
        assert_eq!(r.status, Status::Fail);
        assert!(r.details.iter().any(|d| d.contains("sample1.fcs")));
    }

    #[test]
    fn duplicate_sample_ids_caught() {
        let mut m = base_metadata();
        m.file_data.rows.push(vec!["sample3.fcs".into(), "s1".into(), "A".into()]);
        assert_eq!(check_unique_sample_ids(&m).status, Status::Fail);
    }

    #[test]
    fn sample_id_unicode_warns() {
        let mut m = base_metadata();
        m.file_data.rows[0][1] = "s1²".into();
        assert_eq!(check_sample_id_chars(&m).status, Status::Warn);
    }

    #[test]
    fn blank_condition_fails() {
        let mut m = base_metadata();
        m.file_data.rows[0][2] = "".into();
        let r = check_blank_conditions(&m);
        assert_eq!(r.status, Status::Fail);
        assert!(r.details.iter().any(|d| d.contains("sample1.fcs")));
    }

    #[test]
    fn marker_type_case_sensitive_fail() {
        let mut m = base_metadata();
        m.study_data.rows[0][3] = "Type".into(); // capitalised — rejected now
        assert_eq!(check_marker_types(&m).status, Status::Fail);
    }

    #[test]
    fn marker_type_rejects_invalid_value() {
        let mut m = base_metadata();
        m.study_data.rows[1][3] = "lineage".into();
        assert_eq!(check_marker_types(&m).status, Status::Fail);
    }

    #[test]
    fn marker_in_include_and_exclude_fails() {
        let mut m = base_metadata();
        m.study_data.rows[0][5] = "CD3".into(); // same as include
        assert_eq!(check_marker_include_exclude_disjoint(&m).status, Status::Fail);
    }

    #[test]
    fn cofactor_missing_fails() {
        let mut m = base_metadata();
        m.study_data.rows[0][4] = "".into();
        assert_eq!(check_cofactors(&m).status, Status::Fail);
    }

    #[test]
    fn cofactor_nonnumeric_fails() {
        let mut m = base_metadata();
        m.study_data.rows[0][4] = "five".into();
        assert_eq!(check_cofactors(&m).status, Status::Fail);
    }

    #[test]
    fn contrast_unknown_condition_fails() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A over Z".into();
        assert_eq!(check_contrasts(&m).status, Status::Fail);
    }

    #[test]
    fn contrast_case_mismatch_fails() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "a over b".into();
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Fail);
        assert!(r.details.iter().any(|d| d.contains("case mismatch")));
    }

    #[test]
    fn contrast_self_fails() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A over A".into();
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Fail);
        assert!(r.details.iter().any(|d| d.contains("self-contrast")));
    }

    #[test]
    fn contrast_non_canonical_separator_warns_with_suggestion() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A vs B".into();
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Warn);
        assert!(r.details.iter().any(|d| d.contains("suggest")));
    }

    #[test]
    fn contrast_extra_whitespace_warns_not_fails() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A  over  B".into(); // doubled spaces
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Warn);
        assert!(r.details.iter().any(|d| d.contains("A over B")));
    }

    #[test]
    fn contrast_tab_separator_warns_not_fails() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A\tover\tB".into();
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Warn);
    }

    #[test]
    fn contrast_canonical_does_not_warn() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A over B".into();
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Pass);
    }

    #[test]
    fn contrast_orphan_over_warns_when_other_valid_contrast_present() {
        // Cell contains just "over" — pipeline will filter it. Warn, don't
        // fail, as long as there's at least one real contrast alongside.
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A over B".into();
        m.study_data.rows[1][1] = "over".into();
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Warn);
        assert!(r.details.iter().any(|d| d.contains("ignored by the pipeline")));
    }

    #[test]
    fn contrast_all_unparseable_fails_via_zero_valid() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "over".into();
        m.study_data.rows[1][1] = "garbage".into();
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Fail);
        assert!(r.details.iter().any(|d| d.contains("No contrast survives")));
    }

    #[test]
    fn contrast_empty_half_warns_when_valid_contrast_present() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A over B".into();
        m.study_data.rows[1][1] = "A over ".into(); // trailing missing RHS
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Warn);
    }

    #[test]
    fn contrast_empty_dropdown_pair_emits_specific_message() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A over B".into();
        m.study_data.rows[1][1] = " over ".into(); // both dropdowns unset
        let r = check_contrasts(&m);
        assert_eq!(r.status, Status::Warn);
        assert!(r.details.iter().any(|d| d.contains("dropdowns appear unset")),
            "expected dropdowns-unset message, got: {:?}", r.details);
    }

    #[test]
    fn contrast_raw_whitespace_shown_in_error() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A over B".into();
        m.study_data.rows[1][1] = " over ".into();
        let r = check_contrasts(&m);
        // The error should show the raw cell `" over "` (with the spaces),
        // not a trimmed `"over"` — so users can see the real cell content.
        assert!(r.details.iter().any(|d| d.contains("\" over \"")),
            "expected raw ' over ' in details, got: {:?}", r.details);
    }

    #[test]
    fn contrast_reverse_direction_allowed() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A over B".into();
        m.study_data.rows[1][1] = "B over A".into();
        assert_eq!(check_contrasts(&m).status, Status::Pass);
    }

    #[test]
    fn contrast_duplicate_warns() {
        let mut m = base_metadata();
        m.study_data.rows[0][1] = "A over B".into();
        m.study_data.rows[1][1] = "A over B".into();
        assert_eq!(check_contrasts(&m).status, Status::Warn);
    }

    #[test]
    fn contrast_missing_column_fails() {
        let mut m = base_metadata();
        // Drop the "Conditions To Test" header
        let idx = m.study_data.headers.iter().position(|h| h == "Conditions To Test").unwrap();
        m.study_data.headers.remove(idx);
        for row in m.study_data.rows.iter_mut() { row.remove(idx); }
        assert_eq!(check_contrasts(&m).status, Status::Fail);
    }

    #[test]
    fn contrasts_required_zero_fails() {
        let mut m = base_metadata();
        let idx = m.study_data.headers.iter().position(|h| h == "Conditions To Test").unwrap();
        for row in m.study_data.rows.iter_mut() { row[idx] = "".into(); }
        assert_eq!(check_contrasts(&m).status, Status::Fail);
    }

    #[test]
    fn conditions_order_extra_fails() {
        let mut m = base_metadata();
        let idx = m.study_data.headers.iter().position(|h| h == "Conditions Order").unwrap();
        m.study_data.rows.push({
            let mut r = m.study_data.headers.iter().map(|_| "".to_string()).collect::<Vec<_>>();
            r[idx] = "Z".into();
            r
        });
        assert_eq!(check_conditions_order(&m).status, Status::Fail);
    }

    #[test]
    fn unknown_study_column_warns() {
        let mut m = base_metadata();
        m.study_data.headers.push("Something else".into());
        for row in m.study_data.rows.iter_mut() { row.push("".into()); }
        let results = check_study_data_columns_known(&m);
        assert!(results.iter().any(|r| r.id == "study_data_columns_extra" && r.status == Status::Warn));
    }

    #[test]
    fn misspelled_study_column_fails() {
        let mut m = base_metadata();
        m.study_data.headers.push("markers to include".into()); // wrong case
        for row in m.study_data.rows.iter_mut() { row.push("".into()); }
        let results = check_study_data_columns_known(&m);
        assert!(results.iter().any(|r| r.id == "study_data_columns_typo" && r.status == Status::Fail));
    }

    #[test]
    fn enum_invalid_fails() {
        let mut m = base_metadata();
        m.pipeline_settings.iter_mut().find(|s| s.variable == "runQC").unwrap().setting = "Whatever".into();
        assert_eq!(check_enum(&m, "runQC", RUN_QC_ALLOWED, "x").status, Status::Fail);
    }

    #[test]
    fn numeric_nonpositive_fails() {
        let mut m = base_metadata();
        m.pipeline_settings.iter_mut().find(|s| s.variable == "nCores").unwrap().setting = "0".into();
        assert_eq!(check_numeric(&m, "nCores", false, "x").status, Status::Fail);
    }

    #[test]
    fn useqc_true_with_none_runqc_fails() {
        let mut m = base_metadata();
        m.pipeline_settings.iter_mut().find(|s| s.variable == "useQC").unwrap().setting = "TRUE".into();
        m.pipeline_settings.iter_mut().find(|s| s.variable == "runQC").unwrap().setting = "None".into();
        assert_eq!(check_coherence_qc(&m).status, Status::Fail);
    }

    #[test]
    fn parallel_with_1_core_warns() {
        let mut m = base_metadata();
        m.pipeline_settings.iter_mut().find(|s| s.variable == "nCores").unwrap().setting = "1".into();
        assert_eq!(check_coherence_parallel(&m).status, Status::Warn);
    }

    #[test]
    fn role_type_without_matches_fails() {
        let mut m = base_metadata();
        m.pipeline_settings.iter_mut().find(|s| s.variable == "markersToClusterBy").unwrap().setting = "state".into();
        // Marker Type column has 'type' + 'state' — flip both to 'type'
        let mt_idx = m.study_data.headers.iter().position(|h| h == "Marker Type").unwrap();
        for row in m.study_data.rows.iter_mut() { row[mt_idx] = "type".into(); }
        assert_eq!(
            check_markers_role_has_matches(&m, "markersToClusterBy", "x").status,
            Status::Fail
        );
    }

    #[test]
    fn reload_folder_missing_fails() {
        let mut m = base_metadata();
        m.pipeline_settings.push(mk_setting("RDataFolder", "/nonexistent/path/xyz"));
        assert_eq!(check_reload_folder(&m).status, Status::Fail);
    }

    #[test]
    fn report_summary_counts() {
        let checks = vec![
            CheckResult::pass("a", "A", 1, ""),
            CheckResult::pass("b", "B", 1, ""),
            CheckResult::warn("c", "C", 1, "", vec![]),
            CheckResult::fail("d", "D", 1, "", vec![]),
            CheckResult::skip("e", "E", 1, ""),
        ];
        let r = PreflightReport::from_checks(checks);
        assert_eq!(r.summary.passed, 2);
        assert_eq!(r.summary.warnings, 1);
        assert_eq!(r.summary.failures, 1);
        assert_eq!(r.summary.skipped, 1);
    }

    #[test]
    fn parse_contrast_canonical() {
        let r = parse_contrast("A over B").unwrap();
        assert_eq!(r.0, "A"); assert_eq!(r.1, "B"); assert!(r.2.is_none());
    }

    #[test]
    fn parse_contrast_vs() {
        let r = parse_contrast("A vs B").unwrap();
        assert_eq!(r.0, "A"); assert_eq!(r.1, "B");
        assert_eq!(r.2, Some("A over B".into()));
    }

    #[test]
    fn parse_contrast_versus() {
        let r = parse_contrast("FOO versus BAR").unwrap();
        assert_eq!(r.0, "FOO"); assert_eq!(r.1, "BAR");
    }

    #[test]
    fn parse_contrast_empty_half() {
        assert!(parse_contrast(" over B").is_err());
        assert!(parse_contrast("A over  ").is_err());
    }

    #[test]
    fn parse_contrast_no_sep() {
        assert!(parse_contrast("AoverB").is_err());
        assert!(parse_contrast("A-B").is_err());
    }
}
