// Field definitions for Pipeline Settings
// Each field: { variable, type, options?, label?, info? }
export const FIELD_DEFINITIONS = {
  clusteringMethodToUse: {
    type: "dropdown",
    options: ["MfastPG", "Mphenograph", "Rphenograph", "PARC", "FlowSOM"],
    label: "Clustering Method",
    lockedOnReload: true,
  },
  markersToClusterBy: {
    type: "dropdown",
    options: ["all", "type", "state"],
    label: "Markers to Cluster By",
    lockedOnReload: true,
  },
  kValuesIWant: {
    type: "text",
    label: "Clustering levels",
    lockedOnReload: true,
    placeholder: "e.g. 20, 40",
    info: "Run multiple values to see how cell clustering is affected. Use odd numbers for Rphenograph/MfastPG/PARC, even numbers for FlowSOM. Values between 10\u2013100.",
  },
  knn: {
    type: "dropdown",
    label: "Cluster level to test",
    options: [],
    dynamicOptionsFrom: "kValuesIWant",
    info: "The clustering level that downstream analysis will focus on. Must be one of the Clustering levels above.",
    lockedOnReload: true,
  },
  dimRedMethodToUse: {
    type: "dropdown",
    options: ["TSNE", "UMAP", "pacmap"],
    label: "Dim. Reduction Method",
    lockedOnReload: true,
  },
  markersToDimRedBy: {
    type: "dropdown",
    options: ["all", "type", "state"],
    label: "Markers for Dim. Reduction",
    lockedOnReload: true,
  },
  runQC: {
    type: "dropdown",
    options: ["FlowAI", "PeacoQC", "None"],
    label: "QC Method",
    lockedOnReload: true,
  },
  useQC: {
    type: "checkbox",
    label: "Apply QC Filter",
    lockedOnReload: true,
  },
  downsampleTo: {
    type: "number",
    label: "Downsample To",
    placeholder: "Leave blank = no downsampling",
    allowEmpty: true,
    lockedOnReload: true,
  },
  gimmePDFs: {
    type: "checkbox",
    label: "Export PDFs",
  },
  greyscalePlots: {
    type: "checkbox",
    label: "Greyscale Plots",
  },
  quantileNormaliseAll: {
    type: "checkbox",
    label: "Quantile Normalise",
    lockedOnReload: true,
  },
  runInParallel: {
    type: "checkbox",
    label: "Run in Parallel",
  },
  runScGate: {
    type: "checkbox",
    label: "Run scGate",
    lockedOnReload: true,
  },
  nCores: {
    type: "number",
    label: "CPU Cores",
    min: 1,
  },
  ramPerCore: {
    type: "number",
    label: "RAM per Core (GB)",
    min: 1,
  },
  themeToUse: {
    type: "dropdown",
    options: ["prism", "classic", "bw", "minimal", "void", "light", "dark"],
    label: "Plot Theme",
  },
  viridisColour: {
    type: "dropdown",
    options: [
      "lisbon", "berlin", "vik", "cork", "batlow", "lapaz",
      "magma", "inferno", "plasma", "viridis", "mako", "rocket", "turbo",
    ],
    label: "Colour Palette",
  },
  RDataFolder: {
    type: "folder",
    label: "R Data Folder (reload)",
    placeholder: "Select previous R_files folder to reload...",
  },
  condaDir: {
    type: "folder",
    label: "Conda Directory",
    placeholder: "Path to conda/mamba binary...",
  },
  parcScriptDir: {
    type: "folder",
    label: "PARC Script Directory",
    placeholder: "Path to folder containing f_parc.py and f_pacmap.py...",
  },
};

// Maps optional package/feature name → { field, option } it gates.
// Used by PipelineSettings to disable unavailable dropdown options.
export const PACKAGE_REQUIREMENTS = {
  Rphenograph: { field: "clusteringMethodToUse", option: "Rphenograph" },
  PARC:        { field: "clusteringMethodToUse", option: "PARC"        },
  PeacoQC:     { field: "runQC",                 option: "PeacoQC"     },
  flowAI:      { field: "runQC",                 option: "FlowAI"      },
  pacmap:      { field: "dimRedMethodToUse",      option: "pacmap"      },
};

// Group settings for display
export const SETTING_GROUPS = [
  {
    label: "Clustering",
    fields: ["clusteringMethodToUse", "markersToClusterBy", "kValuesIWant", "knn"],
  },
  {
    label: "Dimension Reduction",
    fields: ["dimRedMethodToUse", "markersToDimRedBy"],
  },
  {
    label: "Quality Control",
    fields: ["runQC", "useQC", "runScGate"],
  },
  {
    label: "Preprocessing",
    fields: ["downsampleTo", "quantileNormaliseAll", "RDataFolder"],
  },
  {
    label: "Parallelism",
    fields: ["runInParallel", "nCores", "ramPerCore"],
  },
  {
    label: "Output & Aesthetics",
    fields: ["gimmePDFs", "greyscalePlots", "themeToUse", "viridisColour"],
  },
  {
    label: "Python / PARC",
    fields: ["condaDir", "parcScriptDir"],
  },
];
