const REQUIRED_FIELDS = [
  "clusteringMethodToUse",
  "markersToClusterBy",
  "kValuesIWant",
  "knn",
  "dimRedMethodToUse",
  "markersToDimRedBy",
  "runQC",
  "useQC",
  "gimmePDFs",
  "quantileNormaliseAll",
  "nCores",
  "ramPerCore",
  "viridisColour",
];

export function validateSettings(settings) {
  const errors = [];

  for (const field of REQUIRED_FIELDS) {
    const setting = settings.find((s) => s.variable === field);
    if (!setting || setting.setting === "" || setting.setting === null || setting.setting === undefined) {
      errors.push(`"${field}" is required and cannot be blank.`);
    }
  }

  // knn must be in kValuesIWant
  const knnSetting = settings.find((s) => s.variable === "knn");
  const kValuesSetting = settings.find((s) => s.variable === "kValuesIWant");
  if (knnSetting && kValuesSetting && knnSetting.setting && kValuesSetting.setting) {
    const kValues = kValuesSetting.setting
      .split(/[,\s]+/)
      .filter((v) => v)
      .map(Number);
    const knn = Number(knnSetting.setting);
    if (!kValues.includes(knn)) {
      errors.push(`Cluster level to test (${knn}) must be one of the Clustering levels (${kValues.join(", ")}).`);
    }
  }

  // Batch correction works with the graph clustering methods only
  const batchSetting = settings.find((s) => s.variable === "batchColumn");
  const methodSetting = settings.find((s) => s.variable === "clusteringMethodToUse");
  const graphMethods = ["MfastPG", "Mphenograph", "Mparc", "PARC", "Rphenograph"];
  if (batchSetting && String(batchSetting.setting ?? "").trim() !== "" &&
      methodSetting && !graphMethods.includes(methodSetting.setting)) {
    errors.push(`Batch correction works with ${graphMethods.join(", ")}, not ${methodSetting.setting}. Choose one of those, or leave the batch column blank.`);
  }

  // Numeric fields must parse correctly
  const numericFields = ["nCores", "ramPerCore", "downsampleTo", "figureDPI"];
  for (const field of numericFields) {
    const setting = settings.find((s) => s.variable === field);
    if (setting && setting.setting !== "" && isNaN(Number(setting.setting))) {
      errors.push(`"${field}" must be a valid number.`);
    }
  }

  return errors;
}

export function validateFileData(fileData) {
  const errors = [];
  if (!fileData.rows || fileData.rows.length === 0) {
    errors.push("File Data must have at least one row.");
  }
  return errors;
}
