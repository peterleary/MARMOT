// Field definitions for Pipeline Settings
// Each field: { variable, type, options?, label?, info? }
export const FIELD_DEFINITIONS = {
  clusteringMethodToUse: {
    type: "dropdown",
    options: ["FlowSOM", "MfastPG", "Mphenograph", "Mparc", "Rphenograph", "PARC"],
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
    placeholder: "e.g. 20, 40, 60",
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
    options: ["UMAP", "TSNE", "Mpacmap", "pacmap"],
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
    placeholder: "Blank = every cell (slower)",
    info: "Cells to take from each FCS file. 10000 gives a quick first look; clear it to use every cell once you are happy.",
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
  viridisColour: {
    type: "grouped-dropdown",
    options: {
      "Viridis": ["magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"],
      "Scico": ["bam", "berlin", "brocO", "corkO", "lapaz", "lisbon", "romaO", "vikO"],
      "Diverging": ["BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"],
      "Fun": ["Marmot", "Swiss", "Lucky"],
    },
    label: "Colour Palette",
  },
  figureDPI: {
    type: "number",
    label: "Figure Resolution (DPI)",
    info: "Resolution of the report's figures, in dots per inch. Higher is sharper but makes the report bigger.",
    min: 50,
  },
  excludeTheseSamples: {
    type: "text",
    label: "Samples to leave out",
    placeholder: "e.g. S001, S026 (blank = none)",
    info: "Sample IDs to leave out of the analysis, separated by commas.",
    lockedOnReload: true,
  },
  RDataFolder: {
    type: "folder",
    label: "R Data Folder (reload)",
    placeholder: "Select previous R_files folder to reload...",
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
    fields: ["downsampleTo", "excludeTheseSamples", "quantileNormaliseAll", "RDataFolder"],
  },
  {
    label: "Resources",
    fields: ["nCores", "ramPerCore"],
  },
  {
    label: "Output & Aesthetics",
    fields: ["gimmePDFs", "greyscalePlots", "viridisColour", "figureDPI"],
  },
];
