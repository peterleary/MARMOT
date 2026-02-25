use calamine::{open_workbook, Reader, Xlsx, Data};
use super::types::{MetadataFile, PipelineSetting, SheetData};

fn cell_to_string(cell: &Data) -> String {
    match cell {
        Data::Empty => String::new(),
        Data::String(s) => s.clone(),
        Data::Float(f) => {
            if *f == (*f as i64) as f64 {
                format!("{}", *f as i64)
            } else {
                format!("{}", f)
            }
        }
        Data::Int(i) => format!("{}", i),
        Data::Bool(b) => format!("{}", b).to_uppercase(),
        Data::DateTime(dt) => format!("{}", dt),
        Data::DateTimeIso(s) => s.clone(),
        Data::DurationIso(s) => s.clone(),
        Data::Error(e) => format!("{:?}", e),
    }
}

fn read_generic_sheet(workbook: &mut Xlsx<std::io::BufReader<std::fs::File>>, sheet_name: &str) -> Result<SheetData, String> {
    let range = workbook.worksheet_range(sheet_name)
        .map_err(|e| format!("Failed to read sheet '{}': {}", sheet_name, e))?;

    let mut headers = Vec::new();
    let mut rows = Vec::new();

    for (i, row) in range.rows().enumerate() {
        if i == 0 {
            headers = row.iter().map(cell_to_string).collect();
        } else {
            let row_data: Vec<String> = row.iter().map(cell_to_string).collect();
            if row_data.iter().any(|c| !c.is_empty()) {
                rows.push(row_data);
            }
        }
    }

    Ok(SheetData { headers, rows })
}

pub fn read_metadata_excel(path: &str) -> Result<MetadataFile, String> {
    let mut workbook: Xlsx<_> = open_workbook(path)
        .map_err(|e| format!("Failed to open Excel file: {}", e))?;

    let sheet_names: Vec<String> = workbook.sheet_names().to_vec();

    // Verify Pipeline Settings sheet exists
    if !sheet_names.iter().any(|s| s.eq_ignore_ascii_case("Pipeline Settings")) {
        return Err("No 'Pipeline Settings' sheet found in metadata file".to_string());
    }

    // Read Pipeline Settings
    let ps_range = workbook.worksheet_range("Pipeline Settings")
        .map_err(|e| format!("Failed to read Pipeline Settings: {}", e))?;

    let mut pipeline_settings = Vec::new();
    let mut headers: Vec<String> = Vec::new();

    for (i, row) in ps_range.rows().enumerate() {
        if i == 0 {
            headers = row.iter().map(cell_to_string).collect();
            continue;
        }
        let cells: Vec<String> = row.iter().map(cell_to_string).collect();
        if cells.iter().all(|c| c.is_empty()) {
            continue;
        }

        let var_idx = headers.iter().position(|h| h == "Variable").unwrap_or(0);
        let set_idx = headers.iter().position(|h| h == "Setting").unwrap_or(1);
        let info_idx = headers.iter().position(|h| h == "Info").unwrap_or(2);

        pipeline_settings.push(PipelineSetting {
            variable: cells.get(var_idx).cloned().unwrap_or_default(),
            setting: cells.get(set_idx).cloned().unwrap_or_default(),
            info: cells.get(info_idx).cloned().unwrap_or_default(),
        });
    }

    // Read Study Data
    let study_data = if sheet_names.iter().any(|s| s == "Study Data") {
        read_generic_sheet(&mut workbook, "Study Data")?
    } else {
        SheetData::new()
    };

    // Read File Data
    let file_data = if sheet_names.iter().any(|s| s == "File Data") {
        read_generic_sheet(&mut workbook, "File Data")?
    } else {
        SheetData::new()
    };

    // Read Options
    let options = if sheet_names.iter().any(|s| s == "Options") {
        read_generic_sheet(&mut workbook, "Options")?
    } else {
        SheetData::new()
    };

    Ok(MetadataFile {
        path: Some(path.to_string()),
        pipeline_settings,
        study_data,
        file_data,
        options,
    })
}
