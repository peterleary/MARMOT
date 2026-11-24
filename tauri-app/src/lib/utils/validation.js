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

// The app's copy of the pipeline's MARMOT::check_batch_design(), so a batch
// design that cannot work is caught before anything runs. Same checks, same
// order; keep the two in step.
export function checkBatchDesign(fileData, batchIdx, column, excluded, key) {
  const sIdx = fileData.headers.indexOf("sample_id");
  const cIdx = fileData.headers.indexOf("condition");
  if (sIdx < 0 || cIdx < 0) return [];
  const rows = (fileData.rows || [])
    .map((r) => ({ sample: String(r[sIdx] ?? "").trim(), cond: key(r[cIdx] ?? ""), batch: String(r[batchIdx] ?? "").trim() }))
    .filter((r) => r.sample && !excluded.includes(key(r.sample)));
  if (rows.length === 0) return [];

  const blank = rows.filter((r) => !r.batch);
  if (blank.length) {
    return [`The batch column "${column}" has no batch for ${blank.length} sample(s): ${blank.map((r) => r.sample).join(", ")}. Every sample needs a batch, or choose None.`];
  }
  const count = (xs) => xs.reduce((m, x) => m.set(x, (m.get(x) || 0) + 1), new Map());
  const perBatch = count(rows.map((r) => r.batch));
  if (perBatch.size < 2) {
    return [`Every sample is in the same batch ("${[...perBatch.keys()][0]}"), so there is nothing to correct for. Choose None.`];
  }
  const single = [...perBatch].filter(([, n]) => n < 2).map(([b]) => b);
  if (single.length) {
    return [`${single.length} batch(es) hold only one sample: ${single.map((b) => `"${b}"`).join(", ")}. A batch of one sample cannot be told apart from that sample itself. Merge it into another batch, or choose None.`];
  }
  const distinct = (by, of) => {
    const m = new Map();
    for (const r of rows) m.set(r[by], (m.get(r[by]) || new Set()).add(r[of]));
    return [...m.values()].map((s) => s.size);
  };
  if (distinct("batch", "cond").every((n) => n === 1) && distinct("cond", "batch").every((n) => n === 1)) {
    return [`Batch and condition are the same thing in this experiment: each batch holds only one condition. The batch effect cannot be separated from the condition effect. Choose None.`];
  }
  const conds = [...new Set(rows.map((r) => r.cond))];
  const batches = [...perBatch.keys()];
  const p = conds.length + batches.length - 1;
  if (rows.length - p < 2) {
    return [`Too few samples (${rows.length}) for ${conds.length} conditions and ${batches.length} batches: fewer than two would be left to estimate the noise. Choose None, or add samples.`];
  }
  // Design ~ 0 + condition + batch must have full column rank
  const design = rows.map((r) => [...conds.map((c) => (r.cond === c ? 1 : 0)), ...batches.slice(1).map((b) => (r.batch === b ? 1 : 0))]);
  if (matrixRank(design) < p) {
    return [`The batches and conditions overlap in a way that leaves some effects impossible to tell apart. Check the batch column "${column}", or choose None.`];
  }
  return [];
}

function matrixRank(m) {
  const a = m.map((r) => r.slice());
  let rank = 0;
  const cols = a[0]?.length || 0;
  for (let c = 0; c < cols && rank < a.length; c++) {
    let piv = rank;
    for (let i = rank + 1; i < a.length; i++) if (Math.abs(a[i][c]) > Math.abs(a[piv][c])) piv = i;
    if (Math.abs(a[piv][c]) < 1e-9) continue;
    [a[rank], a[piv]] = [a[piv], a[rank]];
    for (let i = 0; i < a.length; i++) {
      if (i === rank) continue;
      const f = a[i][c] / a[rank][c];
      for (let j = c; j < cols; j++) a[i][j] -= f * a[rank][j];
    }
    rank++;
  }
  return rank;
}

export function validateSettings(settings, fileData = null) {
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

  // The batch column must be one of the File Data columns (spaces, hyphens,
  // underscores and capitals do not count, as in the pipeline)
  const batchName = String(batchSetting?.setting ?? "").trim();
  if (batchName !== "" && fileData) {
    const key = (x) => String(x).toUpperCase().replace(/[\s_-]/g, "");
    const headers = fileData.headers || [];
    const batchIdx = headers.findIndex((h) => key(h) === key(batchName));
    if (batchIdx < 0) {
      errors.push(`The batch column "${batchName}" is not a File Data column. Add it to File Data, or choose None.`);
    } else {
      const excluded = String(settings.find((s) => s.variable === "excludeTheseSamples")?.setting ?? "")
        .split(",").map((x) => key(x.trim())).filter((x) => x);
      errors.push(...checkBatchDesign(fileData, batchIdx, headers[batchIdx], excluded, key));
    }
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
