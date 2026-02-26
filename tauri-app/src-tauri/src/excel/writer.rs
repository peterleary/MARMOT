use rust_xlsxwriter::{Workbook, Format};
use super::types::MetadataFile;

pub fn write_metadata_excel(metadata: &MetadataFile, path: &str) -> Result<(), String> {
    let mut workbook = Workbook::new();
    let header_format = Format::new().set_bold();

    // Pipeline Settings sheet
    {
        let sheet = workbook.add_worksheet()
            .set_name("Pipeline Settings")
            .map_err(|e| format!("Failed to create sheet: {}", e))?;

        let headers = ["Variable", "Setting", "Info"];
        for (col, h) in headers.iter().enumerate() {
            sheet.write_string_with_format(0, col as u16, *h, &header_format)
                .map_err(|e| format!("Write error: {}", e))?;
        }

        for (i, setting) in metadata.pipeline_settings.iter().enumerate() {
            let row = (i + 1) as u32;
            sheet.write_string(row, 0, &setting.variable)
                .map_err(|e| format!("Write error: {}", e))?;
            sheet.write_string(row, 1, &setting.setting)
                .map_err(|e| format!("Write error: {}", e))?;
            sheet.write_string(row, 2, &setting.info)
                .map_err(|e| format!("Write error: {}", e))?;
        }
    }

    // Study Data sheet
    write_generic_sheet(&mut workbook, "Study Data", &metadata.study_data, &header_format)?;

    // File Data sheet
    write_generic_sheet(&mut workbook, "File Data", &metadata.file_data, &header_format)?;

    // Options sheet
    write_generic_sheet(&mut workbook, "Options", &metadata.options, &header_format)?;

    workbook.save(path)
        .map_err(|e| format!("Failed to save Excel file: {}", e))?;

    Ok(())
}

fn write_generic_sheet(
    workbook: &mut Workbook,
    name: &str,
    data: &super::types::SheetData,
    header_format: &Format,
) -> Result<(), String> {
    let sheet = workbook.add_worksheet()
        .set_name(name)
        .map_err(|e| format!("Failed to create sheet '{}': {}", name, e))?;

    for (col, h) in data.headers.iter().enumerate() {
        sheet.write_string_with_format(0, col as u16, h, header_format)
            .map_err(|e| format!("Write error: {}", e))?;
    }

    for (i, row) in data.rows.iter().enumerate() {
        let row_num = (i + 1) as u32;
        for (col, cell) in row.iter().enumerate() {
            sheet.write_string(row_num, col as u16, cell)
                .map_err(|e| format!("Write error: {}", e))?;
        }
    }

    Ok(())
}
