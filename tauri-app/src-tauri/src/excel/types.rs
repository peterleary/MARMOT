use serde::{Deserialize, Deserializer, Serialize};

/// Deserialize any JSON value (string, number, bool, null) into a String.
/// This handles the case where the frontend sends numbers as JSON integers
/// (e.g. 1000 instead of "1000") from number inputs.
fn deserialize_string_or_number<'de, D>(deserializer: D) -> Result<String, D::Error>
where
    D: Deserializer<'de>,
{
    let v: serde_json::Value = Deserialize::deserialize(deserializer)?;
    Ok(match v {
        serde_json::Value::String(s) => s,
        serde_json::Value::Number(n) => n.to_string(),
        serde_json::Value::Bool(b) => b.to_string().to_uppercase(),
        serde_json::Value::Null => String::new(),
        other => other.to_string(),
    })
}

fn deserialize_rows<'de, D>(deserializer: D) -> Result<Vec<Vec<String>>, D::Error>
where
    D: Deserializer<'de>,
{
    let rows: Vec<Vec<serde_json::Value>> = Deserialize::deserialize(deserializer)?;
    Ok(rows
        .into_iter()
        .map(|row| {
            row.into_iter()
                .map(|v| match v {
                    serde_json::Value::String(s) => s,
                    serde_json::Value::Number(n) => n.to_string(),
                    serde_json::Value::Bool(b) => b.to_string().to_uppercase(),
                    serde_json::Value::Null => String::new(),
                    other => other.to_string(),
                })
                .collect()
        })
        .collect())
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineSetting {
    pub variable: String,
    #[serde(deserialize_with = "deserialize_string_or_number")]
    pub setting: String,
    pub info: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SheetData {
    pub headers: Vec<String>,
    #[serde(deserialize_with = "deserialize_rows")]
    pub rows: Vec<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetadataFile {
    pub path: Option<String>,
    pub pipeline_settings: Vec<PipelineSetting>,
    pub study_data: SheetData,
    pub file_data: SheetData,
    pub options: SheetData,
}

impl SheetData {
    pub fn new() -> Self {
        Self {
            headers: Vec::new(),
            rows: Vec::new(),
        }
    }
}
