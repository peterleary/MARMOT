// The MARMOT extension for annotated cells: what a results folder holds for it.
//
// A first run writes MARMOT_Cluster_Labels.xlsx and a copy of its metadata
// sheet into its results folder. The user names clusters (in Excel or the
// Explorer), then the extension reruns from those results with RDataFolder
// set. This reads the folder so the app can say where things stand.

use calamine::{open_workbook, Data, Reader, Xlsx};
use serde::Serialize;
use std::collections::HashSet;
use std::path::Path;

#[derive(Serialize, Default)]
pub struct ExtensionSummary {
    /// Does the folder look like MARMOT results (an R_files folder)?
    pub is_results: bool,
    /// The labels sheet, if there is one
    pub labels_path: Option<String>,
    pub n_clusters: usize,
    pub n_named: usize,
    pub n_populations: usize,
    pub n_dropped: usize,
    /// The first run's metadata sheet, copied into the results folder
    pub metadata_path: Option<String>,
    /// Where the extension's results will go (the folder above the results)
    pub parent_dir: Option<String>,
}

fn cell_text(cell: &Data) -> String {
    match cell {
        Data::Empty => String::new(),
        Data::String(s) => s.trim().to_string(),
        Data::Float(f) if *f == (*f as i64) as f64 => format!("{}", *f as i64),
        Data::Float(f) => format!("{}", f),
        Data::Int(i) => format!("{}", i),
        other => format!("{}", other),
    }
}

/// MARMOT's name rule: capitals, spaces, hyphens and underscores do not count.
fn name_key(s: &str) -> String {
    s.chars()
        .filter(|c| !c.is_whitespace() && *c != '-' && *c != '_')
        .collect::<String>()
        .to_uppercase()
}

#[tauri::command]
pub fn read_extension_summary(results_dir: String) -> Result<ExtensionSummary, String> {
    let dir = Path::new(&results_dir);
    if !dir.is_dir() {
        return Err(format!("{} is not a folder.", results_dir));
    }
    let mut out = ExtensionSummary {
        is_results: dir.join("R_files").is_dir(),
        parent_dir: dir.parent().map(|p| p.to_string_lossy().to_string()),
        ..Default::default()
    };

    // The metadata copy: any .xlsx in the folder that is not a labels sheet
    if let Ok(entries) = std::fs::read_dir(dir) {
        let mut sheets: Vec<String> = entries
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.extension().map(|x| x == "xlsx").unwrap_or(false))
            .filter(|p| {
                let n = p.file_name().unwrap_or_default().to_string_lossy().to_string();
                !n.starts_with("MARMOT_Cluster_Labels") && !n.starts_with("~$")
            })
            .map(|p| p.to_string_lossy().to_string())
            .collect();
        sheets.sort();
        out.metadata_path = sheets.into_iter().next();
    }

    let labels = dir.join("MARMOT_Cluster_Labels.xlsx");
    if labels.is_file() {
        out.labels_path = Some(labels.to_string_lossy().to_string());
        let mut wb: Xlsx<_> = open_workbook(&labels).map_err(|e| format!("Cannot open the labels sheet: {}", e))?;
        let range = wb
            .worksheet_range("Cluster Labels")
            .map_err(|e| format!("The labels sheet has no 'Cluster Labels' tab: {}", e))?;
        let mut rows = range.rows();
        let header: Vec<String> = rows.next().map(|r| r.iter().map(cell_text).collect()).unwrap_or_default();
        let ci = header.iter().position(|h| h == "Cluster");
        let li = header.iter().position(|h| h == "Label");
        if let (Some(ci), Some(li)) = (ci, li) {
            let mut populations = HashSet::new();
            for r in rows {
                let cluster = r.get(ci).map(cell_text).unwrap_or_default();
                if cluster.is_empty() {
                    continue;
                }
                out.n_clusters += 1;
                let label = r.get(li).map(cell_text).unwrap_or_default();
                if label.is_empty() {
                    continue;
                }
                let key = name_key(&label);
                if key == "DROP" {
                    out.n_dropped += 1;
                } else {
                    out.n_named += 1;
                    populations.insert(key);
                }
            }
            out.n_populations = populations.len();
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::{name_key, read_extension_summary};

    #[test]
    fn reads_a_results_folder() {
        let dir = std::env::temp_dir().join(format!("marmot_ext_test_{}", std::process::id()));
        let res = dir.join("Results_Files_x");
        std::fs::create_dir_all(res.join("R_files")).unwrap();
        let mut wb = rust_xlsxwriter::Workbook::new();
        let ws = wb.add_worksheet().set_name("Cluster Labels").unwrap();
        let rows = [("Cluster", "Label"), ("1", "T cells"), ("2", ""), ("3", "t-cells"), ("4", "drop"), ("5", "B cells")];
        for (i, (c, l)) in rows.iter().enumerate() {
            ws.write_string(i as u32, 0, *c).unwrap();
            ws.write_string(i as u32, 4, *l).unwrap();
            if i == 0 { ws.write_string(0, 1, "Cells").unwrap(); }
        }
        // the Label column is the fifth, as in MARMOT's sheet; make the header say so
        ws.write_string(0, 4, "Label").unwrap();
        wb.save(res.join("MARMOT_Cluster_Labels.xlsx")).unwrap();
        rust_xlsxwriter::Workbook::new().save(res.join("My_Metadata.xlsx")).unwrap();
        let s = read_extension_summary(res.to_string_lossy().to_string()).unwrap();
        assert!(s.is_results);
        assert_eq!((s.n_clusters, s.n_named, s.n_populations, s.n_dropped), (5, 3, 2, 1));
        assert!(s.metadata_path.unwrap().ends_with("My_Metadata.xlsx"));
        assert_eq!(s.parent_dir.unwrap(), dir.to_string_lossy());
        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn names_match_forgivingly() {
        assert_eq!(name_key("T cells"), name_key("t-cells"));
        assert_eq!(name_key("T_Cells"), "TCELLS");
        assert_eq!(name_key(" drop "), "DROP");
    }
}
