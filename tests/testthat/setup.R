# Shared test fixtures for MARMOT tests

library(ggplot2)

# Source helpers (not exported package functions)
helpers_dir <- system.file("app", "helpers", package = "MARMOT")
source(file.path(helpers_dir, "data_helpers.R"), local = TRUE)
source(file.path(helpers_dir, "colour_helpers.R"), local = TRUE)
source(file.path(helpers_dir, "plot_helpers.R"), local = TRUE)

#' Create a lightweight mock SingleCellExperiment
#' @param n_cells Number of cells (default 100)
#' @param n_markers Number of markers (default 5)
#' @param n_samples Number of samples (default 2)
#' @return A SingleCellExperiment object
make_mock_sce <- function(n_cells = 100, n_markers = 5, n_samples = 2) {
  set.seed(42)

  counts_mat <- matrix(rpois(n_markers * n_cells, 500), nrow = n_markers)
  expr_mat <- asinh(counts_mat / 150)
  qnorm_mat <- t(apply(expr_mat, 1, function(x) {
    rng <- max(x) - min(x)
    if (rng == 0) return(rep(0, length(x)))
    (x - min(x)) / rng
  }))

  markers <- paste0("Marker", seq_len(n_markers))
  cells <- paste0("cell", seq_len(n_cells))
  rownames(counts_mat) <- rownames(expr_mat) <- rownames(qnorm_mat) <- markers
  colnames(counts_mat) <- colnames(expr_mat) <- colnames(qnorm_mat) <- cells

  sample_ids <- factor(rep(paste0("Sample", seq_len(n_samples)), each = n_cells / n_samples))
  conditions <- factor(ifelse(as.integer(sample_ids) %% 2 == 1, "Control", "Treatment"))
  cluster_ids <- factor(sample(paste0("c", 1:5), n_cells, replace = TRUE))

  col_data <- S4Vectors::DataFrame(
    sample_id = sample_ids,
    condition = conditions,
    cluster_id = cluster_ids,
    row.names = cells
  )

  umap_coords <- matrix(rnorm(n_cells * 2), ncol = 2,
                         dimnames = list(cells, c("UMAP1", "UMAP2")))

  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = counts_mat, exprsTransformed = expr_mat, exprsQuantNorm = qnorm_mat),
    colData = col_data,
    reducedDims = list(UMAP = umap_coords)
  )

  md <- data.frame(
    sample_id = levels(sample_ids),
    condition = c("Control", "Treatment"),
    file_name = paste0(levels(sample_ids), ".fcs"),
    stringsAsFactors = FALSE
  )
  S4Vectors::metadata(sce)$experiment_info <- md

  sce
}

#' Create synthetic FCS files + metadata Excel for pipeline integration test
#'
#' Produces a temp directory with:
#' - 4 FCS files (n_cells each, n_markers markers)
#' - A metadata Excel with Pipeline Settings, File Data, Study Data sheets
#'
#' @param n_cells Cells per FCS file (default 500)
#' @param n_markers Number of markers (default 8)
#' @return Path to the temp directory containing FCS + metadata
make_test_pipeline_data <- function(n_cells = 500, n_markers = 8, params = list(),
                                    marker_types = NULL) {
  set.seed(123)
  tmp <- tempfile("marmot_inttest_")
  dir.create(tmp, recursive = TRUE)

  marker_names <- paste0("Marker_", seq_len(n_markers))
  channel_names <- paste0("Ch", seq_len(n_markers))
  sample_ids <- paste0("Sample_", sprintf("%03d", 1:4))
  conditions <- rep(c("Control", "Treatment"), each = 2)
  file_names <- paste0(sample_ids, ".fcs")

  # Create 4 synthetic FCS files
  for (i in seq_along(sample_ids)) {
    # Simulate expression data with slight condition shift
    mat <- matrix(
      abs(rnorm(n_cells * n_markers, mean = 500, sd = 200)),
      nrow = n_cells, ncol = n_markers
    )
    if (conditions[i] == "Treatment") {
      mat[, 1:2] <- mat[, 1:2] * 1.5  # upregulate first 2 markers
    }
    colnames(mat) <- channel_names

    # Build parameter annotation
    fcs_params <- S4Vectors::DataFrame(
      name = channel_names,
      desc = marker_names,
      range = rep(4096, n_markers),
      minRange = rep(0, n_markers),
      maxRange = rep(4096, n_markers)
    )
    rownames(fcs_params) <- paste0("$P", seq_len(n_markers))

    ff <- flowCore::flowFrame(
      exprs = mat,
      parameters = Biobase::AnnotatedDataFrame(as.data.frame(fcs_params))
    )
    flowCore::write.FCS(ff, file.path(tmp, file_names[i]))
  }

  # Create metadata Excel
  # Sheet 1: Pipeline Settings
  settings <- data.frame(
    Variable = c(
      "clusteringMethodToUse", "markersToClusterBy", "kValuesIWant", "knn",
      "dimRedMethodToUse", "markersToDimRedBy",
      "runQC", "useQC",
      "downsampleTo", "RDataFolder", "excludeTheseSamples",
      "gimmePDFs", "greyscalePlots", "quantileNormaliseAll",
      "runInParallel", "nCores", "ramPerCore",
      "themeToUse", "viridisColour",
      "runScGate"
    ),
    Setting = c(
      "FlowSOM", "all", "10", "10",
      "UMAP", "all",
      "None", "FALSE",
      NA, NA, NA,
      "FALSE", "FALSE", "FALSE",
      "FALSE", "1", "4",
      "prism", "viridis",
      "FALSE"
    ),
    stringsAsFactors = FALSE
  )

  # Merge parameter overrides
  for (nm in names(params)) {
    idx <- which(settings$Variable == nm)
    if (length(idx) == 1) {
      settings$Setting[idx] <- params[[nm]]
    }
  }

  # Sheet 2: File Data
  file_data <- data.frame(
    file_name = file_names,
    sample_id = sample_ids,
    condition = conditions,
    stringsAsFactors = FALSE
  )

  # Sheet 3: Study Data — must have specific columns
  # The pipeline reads: "Markers to include for clustering", "Marker Type",
  # "Markers to exclude completely", "Cofactors for markers to use",
  # "Conditions To Test", "Conditions Order", "Cells per condition in UMAPs etc."
  n_rows <- max(n_markers, 4)  # pad to enough rows
  study_data <- data.frame(
    `Markers to include for clustering` = c(marker_names, rep(NA, n_rows - n_markers)),
    `Marker Type` = c(
      if (is.null(marker_types)) {
        c(rep("type", ceiling(n_markers / 2)),
          rep("state", n_markers - ceiling(n_markers / 2)))
      } else {
        stopifnot(length(marker_types) == n_markers)
        marker_types
      },
      rep(NA, n_rows - n_markers)),
    `Markers to exclude completely` = rep(NA, n_rows),
    `Cofactors for markers to use` = c(rep(150, n_markers), rep(NA, n_rows - n_markers)),
    `Conditions To Test` = c("Treatment over Control", rep(NA, n_rows - 1)),
    `Conditions Order` = c("Control", "Treatment", rep(NA, n_rows - 2)),
    `Cells per condition in UMAPs etc.` = c(
      as.character(n_cells * 2), as.character(n_cells * 2),
      rep(NA, n_rows - 2)
    ),
    `Marker Pairs` = rep(NA, n_rows),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", settings)
  openxlsx::addWorksheet(wb, "File Data")
  openxlsx::writeData(wb, "File Data", file_data)
  openxlsx::addWorksheet(wb, "Study Data")
  openxlsx::writeData(wb, "Study Data", study_data)
  meta_path <- file.path(tmp, "MARMOT_metadata.xlsx")
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)

  tmp
}

#' Create a mock umapDF from a mock SCE
#' @param sce A SingleCellExperiment
#' @return A data.frame with x, y, metadata, and expression columns
make_mock_umap_df <- function(sce) {
  cd <- as.data.frame(SummarizedExperiment::colData(sce))
  expr <- t(as.matrix(SummarizedExperiment::assay(sce, "exprsQuantNorm")))
  rd <- SingleCellExperiment::reducedDim(sce, "UMAP")
  df <- cbind(cd, data.frame(x = rd[, 1], y = rd[, 2]), as.data.frame(expr))
  df
}

#' Create a mock colours list from a mock SCE
#' @param sce A SingleCellExperiment
#' @return Named list of named colour vectors
make_mock_colours <- function(sce) {
  clusters <- levels(sce$cluster_id)
  conds <- levels(sce$condition)
  list(
    cluster_id = setNames(scales::hue_pal()(length(clusters)), clusters),
    condition = setNames(c("#E41A1C", "#377EB8"), conds)
  )
}

#' Create a mock SCE for cell identity matching tests
#'
#' Extends make_mock_sce with a second reducedDim (TSNE), cluster_codes
#' in metadata, and optional NA UMAP coordinates.
#'
#' @param n_cells Number of cells (default 100)
#' @param n_markers Number of markers (default 5)
#' @param n_samples Number of samples (default 2)
#' @param n_na_coords Number of cells to set NA UMAP coordinates (default 0)
#' @return A SingleCellExperiment object
make_cell_matching_sce <- function(n_cells = 100, n_markers = 5, n_samples = 2,
                                   n_na_coords = 0) {
  sce <- make_mock_sce(n_cells = n_cells, n_markers = n_markers, n_samples = n_samples)

  # Add TSNE reducedDim
  set.seed(99)
  cells <- colnames(sce)
  tsne_coords <- matrix(rnorm(n_cells * 2, sd = 10), ncol = 2,
                         dimnames = list(cells, c("TSNE1", "TSNE2")))
  SingleCellExperiment::reducedDim(sce, "TSNE") <- tsne_coords

  # Add cluster_codes to metadata (mimics CATALYST FlowSOM output)
  cluster_levels <- levels(sce$cluster_id)
  cluster_codes_df <- data.frame(
    meta10 = cluster_levels,
    stringsAsFactors = FALSE
  )
  rownames(cluster_codes_df) <- cluster_levels
  S4Vectors::metadata(sce)$cluster_codes <- cluster_codes_df

  # Optionally introduce NA coordinates

  if (n_na_coords > 0 && n_na_coords < n_cells) {
    umap <- SingleCellExperiment::reducedDim(sce, "UMAP")
    na_idx <- seq_len(n_na_coords)
    umap[na_idx, ] <- NA
    SingleCellExperiment::reducedDim(sce, "UMAP") <- umap
  }

  sce
}

#' Create realistic synthetic FCS data for pipeline testing
#'
#' Produces a temp directory with 12 FCS files (4 conditions x 3 replicates)
#' and a metadata Excel workbook. Uses a 21-marker immune panel with 17 cell
#' populations and known differential abundance across conditions.
#'
#' Expression is generated in arcsinh(x/2500) space using real per-marker
#' distribution parameters from Org19 data. Includes spectral bleed,
#' within-population jitter, 22% transitional cells, and 8% doublets.
#'
#' @param n_cells Cells per FCS file (default 5000)
#' @param params Named list of Pipeline Settings overrides
#' @return Path to the temp directory containing FCS + metadata
make_realistic_pipeline_data <- function(n_cells = 5000, params = list(),
                                          marker_types = NULL) {
  set.seed(42)
  tmp <- tempfile("marmot_realistic_")
  dir.create(tmp, recursive = TRUE)

  cofactor <- 2500

  # --- Channel table (30 channels) ---
  channels <- data.frame(
    name = c(
      "FSC-H", "FSC-A", "SSC-H", "SSC-A", "SSC-B-H", "SSC-B-A",
      "FJComp-BUV496-A", "FJComp-BUV661-A", "FJComp-BUV737-A",
      "FJComp-BV421-A", "FJComp-BV510-A", "FJComp-BV605-A",
      "FJComp-BV650-A", "FJComp-BV711-A", "FJComp-BV785-A",
      "FJComp-BB515-A", "FJComp-BB700-A", "FJComp-PE-A",
      "FJComp-PE-CF594-A", "FJComp-PE-Cy5-A", "FJComp-PE-Cy7-A",
      "FJComp-APC-A", "FJComp-APC-R700-A", "FJComp-APC-Fire750-A",
      "FJComp-AF647-A", "FJComp-BUV395-A", "FJComp-BV750-A",
      "Time", "FJComp-Zombie-NIR-A", "FJComp-APC-Fire810-A"
    ),
    desc = c(
      "FSC-H", "FSC-A", "SSC-H", "SSC-A", "SSC-B-H", "SSC-B-A",
      "CD4", "CD8a", "MHCII", "CD19", "Ly6G", "CD103",
      "CD11b", "F480", "NK11", "FoxP3", "TCRb", "CD11c",
      "Ly6C", "KLRG1", "CD44", "LAG3", "Ki67", "PD1",
      "CD25", "TIM3", "ICOS", "Time", "LD", "CD45"
    ),
    range    = c(rep(262144, 6), rep(262144, 21), 262144, 262144, 262144),
    minRange = c(rep(0, 6), rep(-500, 21), 0, -500, -500),
    maxRange = c(rep(262144, 6), rep(262144, 21), 262144, 262144, 262144),
    stringsAsFactors = FALSE
  )

  # Marker subsets
  bio_markers <- c(
    "CD4", "CD8a", "MHCII", "CD19", "Ly6G", "CD103",
    "CD11b", "F480", "NK11", "FoxP3", "TCRb", "CD11c",
    "Ly6C", "KLRG1", "CD44", "LAG3", "Ki67", "PD1",
    "CD25", "TIM3", "ICOS"
  )
  n_bio <- length(bio_markers)
  type_markers <- c("CD8a", "MHCII", "CD4", "CD19", "Ly6G", "CD103",
                     "CD11b", "F480", "NK11", "FoxP3", "TCRb", "CD11c", "Ly6C")
  state_markers <- c("KLRG1", "CD44", "LAG3", "Ki67", "PD1", "CD25", "TIM3", "ICOS")
  excluded_markers <- c("Time", "SSC-H", "SSC-A", "FSC-H", "FSC-A",
                         "SSC-B-H", "SSC-B-A", "LD", "CD45")

  # Map bio_marker name -> column index in channels table
  bio_col_idx <- match(bio_markers, channels$desc)

  # --- Per-marker distribution parameters (arcsinh space, cofactor=2500) ---
  # Extracted from real Org19 data (export_A1, 510K cells) via density peak
  # analysis. Each marker has its own neg/pos means and SDs — no abstract
  # "expression levels". The "lo" and "hi" levels are derived per-marker.
  #
  # Marker order: CD4 CD8a MHCII CD19 Ly6G CD103 CD11b F480 NK11 FoxP3
  #               TCRb CD11c Ly6C KLRG1 CD44 LAG3 Ki67 PD1 CD25 TIM3 ICOS
  marker_params <- data.frame(
    marker   = bio_markers,
    neg_mean = c( 0.56,  0.47,  0.16,  0.72,  0.19, -0.14,  0.95,  0.26,
                 -0.02,  0.83,  0.28,  0.14,  0.16,  0.05,  0.39, -0.02,
                  1.30,  0.61,  0.41,  0.54,  0.25),
    neg_sd   = c( 0.49,  0.33,  0.49,  0.35,  0.21,  0.21,  0.56,  0.30,
                  0.15,  0.30,  0.25,  0.20,  0.27,  0.44,  0.30,  0.15,
                  0.35,  0.20,  0.18,  0.25,  0.25),
    pos_mean = c( 3.04,  2.59,  2.51,  3.22,  2.38,  0.70,  4.66,  1.27,
                  0.42,  2.22,  3.09,  0.95,  2.39,  2.55,  2.27,  0.67,
                  3.19,  1.45,  1.51,  1.07,  0.77),
    pos_sd   = c( 0.43,  0.32,  0.76,  0.30,  1.04,  0.74,  0.52,  0.35,
                  0.51,  0.50,  0.69,  0.50,  1.14,  0.50,  0.50,  0.30,
                  0.50,  0.40,  0.35,  0.30,  0.45),
    row.names = bio_markers,
    stringsAsFactors = FALSE
  )

  # --- 17 populations x 21 markers expression profile ---
  # More sub-populations for realistic heterogeneity and more FlowSOM clusters
  pop_names <- c(
    "CD8_Naive", "CD8_Effector", "CD8_Exhausted", "CD8_RM",
    "CD4_Naive", "CD4_Activated", "CD4_Th1", "Tregs",
    "B_cells", "Plasma_B",
    "Neutrophils", "Mono_Classical", "Mono_Nonclassical", "Macrophages",
    "NK_cells", "cDCs", "pDCs"
  )
  n_pops <- length(pop_names)

  profile_mat <- matrix("neg", nrow = n_pops, ncol = n_bio,
                         dimnames = list(pop_names, bio_markers))

  # CD8+ Naive T cells: CD8a+, TCRb+, CD44-lo, ICOS-lo
  profile_mat["CD8_Naive", c("CD8a", "TCRb")] <- "pos"
  profile_mat["CD8_Naive", "CD44"] <- "lo"
  profile_mat["CD8_Naive", "ICOS"] <- "lo"

  # CD8+ Effector T cells: CD8a+, TCRb+, CD44-hi, KLRG1-lo, Ki67-lo, ICOS-lo
  profile_mat["CD8_Effector", c("CD8a", "TCRb")] <- "pos"
  profile_mat["CD8_Effector", "CD44"] <- "hi"
  profile_mat["CD8_Effector", c("KLRG1", "Ki67", "ICOS")] <- "lo"

  # CD8+ Exhausted: CD8a+, TCRb+, PD1+, TIM3+, LAG3-lo, CD44-hi, ICOS-lo
  profile_mat["CD8_Exhausted", c("CD8a", "TCRb", "PD1", "TIM3")] <- "pos"
  profile_mat["CD8_Exhausted", "LAG3"] <- "lo"
  profile_mat["CD8_Exhausted", "CD44"] <- "hi"
  profile_mat["CD8_Exhausted", "ICOS"] <- "lo"

  # CD8+ Resident Memory: CD8a+, TCRb+, CD44-hi, CD103+, ICOS-lo
  profile_mat["CD8_RM", c("CD8a", "TCRb", "CD103")] <- "pos"
  profile_mat["CD8_RM", "CD44"] <- "hi"
  profile_mat["CD8_RM", "ICOS"] <- "lo"

  # CD4+ Naive T helper: CD4+, TCRb+, CD44-lo, ICOS-lo
  profile_mat["CD4_Naive", c("CD4", "TCRb")] <- "pos"
  profile_mat["CD4_Naive", "CD44"] <- "lo"
  profile_mat["CD4_Naive", "ICOS"] <- "lo"

  # CD4+ Activated T helper: CD4+, TCRb+, CD44+, ICOS+, PD1-lo, Ki67-lo
  profile_mat["CD4_Activated", c("CD4", "TCRb", "ICOS")] <- "pos"
  profile_mat["CD4_Activated", "CD44"] <- "pos"
  profile_mat["CD4_Activated", c("PD1", "Ki67")] <- "lo"

  # CD4+ Th1 hyperactivated: CD4+, TCRb+, CD44-hi, ICOS+, Ki67+, CD25-lo
  profile_mat["CD4_Th1", c("CD4", "TCRb", "ICOS", "Ki67")] <- "pos"
  profile_mat["CD4_Th1", "CD44"] <- "hi"
  profile_mat["CD4_Th1", "CD25"] <- "lo"

  # Tregs: CD4+, FoxP3+, CD25+, TCRb+, ICOS+, CD44-pos
  profile_mat["Tregs", c("CD4", "FoxP3", "CD25", "TCRb", "ICOS")] <- "pos"
  profile_mat["Tregs", "CD44"] <- "pos"

  # B cells: CD19-hi, MHCII-hi, CD44-pos, ICOS-pos, CD103-lo
  profile_mat["B_cells", c("CD19", "MHCII")] <- "hi"
  profile_mat["B_cells", "CD44"] <- "pos"
  profile_mat["B_cells", "ICOS"] <- "pos"
  profile_mat["B_cells", "CD103"] <- "lo"

  # Plasma B cells: CD19-lo (downregulated), MHCII+, Ki67+, CD44-pos, ICOS-lo
  profile_mat["Plasma_B", c("MHCII", "Ki67")] <- "pos"
  profile_mat["Plasma_B", "CD19"] <- "lo"
  profile_mat["Plasma_B", "CD44"] <- "pos"
  profile_mat["Plasma_B", "ICOS"] <- "lo"

  # Neutrophils: Ly6G-hi, CD11b+, Ly6C-lo, CD44-pos, CD103-lo, ICOS-lo
  profile_mat["Neutrophils", "Ly6G"] <- "hi"
  profile_mat["Neutrophils", "CD11b"] <- "pos"
  profile_mat["Neutrophils", "Ly6C"] <- "lo"
  profile_mat["Neutrophils", c("CD44", "ICOS")] <- c("pos", "lo")
  profile_mat["Neutrophils", "CD103"] <- "lo"

  # Classical Monocytes: Ly6C-hi, CD11b-lo, CD44+, F480-lo, CD103-lo, ICOS-lo
  profile_mat["Mono_Classical", "CD44"] <- "pos"
  profile_mat["Mono_Classical", "CD11b"] <- "lo"
  profile_mat["Mono_Classical", "Ly6C"] <- "hi"
  profile_mat["Mono_Classical", c("F480", "CD103", "ICOS")] <- "lo"

  # Non-classical Monocytes: Ly6C-lo, CD11b-lo, MHCII+, CD44+, CD11c-lo, ICOS-lo
  profile_mat["Mono_Nonclassical", c("MHCII", "CD44")] <- "pos"
  profile_mat["Mono_Nonclassical", "CD11b"] <- "lo"
  profile_mat["Mono_Nonclassical", c("Ly6C", "CD11c", "ICOS")] <- "lo"

  # Macrophages: F480+, CD11b+, MHCII-lo, CD11c-lo, Ly6C-lo, CD44+, CD103-lo, ICOS-lo
  profile_mat["Macrophages", c("F480", "CD11b")] <- "pos"
  profile_mat["Macrophages", c("MHCII", "CD11c", "Ly6C")] <- "lo"
  profile_mat["Macrophages", "CD44"] <- "pos"
  profile_mat["Macrophages", c("CD103", "ICOS")] <- "lo"

  # NK cells: NK11+, KLRG1-lo, CD44-hi, CD8a-lo, ICOS-lo
  profile_mat["NK_cells", "NK11"] <- "pos"
  profile_mat["NK_cells", c("KLRG1", "ICOS")] <- "lo"
  profile_mat["NK_cells", "CD44"] <- "hi"
  profile_mat["NK_cells", "CD8a"] <- "lo"

  # Conventional DCs: CD11c-hi, MHCII-hi, CD103+, CD44-pos, ICOS-lo
  profile_mat["cDCs", c("CD11c", "MHCII")] <- "hi"
  profile_mat["cDCs", "CD103"] <- "pos"
  profile_mat["cDCs", c("CD44", "ICOS")] <- c("pos", "lo")

  # Plasmacytoid DCs: CD11c-lo, MHCII+, CD19-lo, CD44-lo, ICOS-lo
  profile_mat["pDCs", "MHCII"] <- "pos"
  profile_mat["pDCs", c("CD11c", "CD19")] <- "lo"
  profile_mat["pDCs", c("CD44", "ICOS")] <- "lo"

  # --- Transition definitions ---
  # 20 pairs with high fractions for realistic inter-population continuum
  transitions <- list(
    list(from = "CD8_Naive",          to = "CD8_Effector",       frac = 0.10),
    list(from = "CD8_Effector",       to = "CD8_Exhausted",      frac = 0.08),
    list(from = "CD8_Naive",          to = "CD8_Exhausted",      frac = 0.04),
    list(from = "CD8_Effector",       to = "CD8_RM",             frac = 0.06),
    list(from = "CD8_Naive",          to = "CD8_RM",             frac = 0.03),
    list(from = "CD4_Naive",          to = "CD4_Activated",      frac = 0.10),
    list(from = "CD4_Activated",      to = "CD4_Th1",            frac = 0.08),
    list(from = "CD4_Activated",      to = "Tregs",              frac = 0.06),
    list(from = "CD4_Naive",          to = "Tregs",              frac = 0.03),
    list(from = "CD4_Th1",            to = "Tregs",              frac = 0.04),
    list(from = "B_cells",            to = "Plasma_B",           frac = 0.06),
    list(from = "Mono_Classical",     to = "Mono_Nonclassical",  frac = 0.10),
    list(from = "Mono_Classical",     to = "Macrophages",        frac = 0.08),
    list(from = "Mono_Nonclassical",  to = "Macrophages",        frac = 0.06),
    list(from = "Macrophages",        to = "cDCs",               frac = 0.05),
    list(from = "cDCs",               to = "pDCs",               frac = 0.04),
    list(from = "NK_cells",           to = "CD8_Effector",       frac = 0.04),
    list(from = "NK_cells",           to = "CD8_Naive",          frac = 0.03),
    list(from = "Neutrophils",        to = "Mono_Classical",     frac = 0.04),
    list(from = "pDCs",               to = "B_cells",            frac = 0.03)
  )

  # --- Differential abundance (proportions per condition) ---
  # B cells dominate (~35% baseline, matching real Org19 spleen data).
  # Strong DA: CD8_Exhausted in KO_treated, Tregs in KO_untreated,
  # B_cells+Plasma_B in WT_treated. Each column sums to 1.0.
  #                              WT_unt  WT_trt  KO_unt  KO_trt
  freq_mat <- matrix(c(
    0.06, 0.05, 0.04, 0.03,  # CD8_Naive
    0.03, 0.02, 0.02, 0.02,  # CD8_Effector
    0.01, 0.01, 0.03, 0.15,  # CD8_Exhausted  *** KO_treated
    0.02, 0.02, 0.01, 0.01,  # CD8_RM
    0.06, 0.05, 0.04, 0.03,  # CD4_Naive
    0.03, 0.02, 0.02, 0.02,  # CD4_Activated
    0.01, 0.01, 0.01, 0.02,  # CD4_Th1
    0.02, 0.02, 0.15, 0.03,  # Tregs          *** KO_untreated
    0.35, 0.40, 0.30, 0.28,  # B_cells        *** WT_treated
    0.02, 0.05, 0.02, 0.02,  # Plasma_B
    0.10, 0.08, 0.10, 0.10,  # Neutrophils
    0.06, 0.05, 0.05, 0.05,  # Mono_Classical
    0.03, 0.03, 0.03, 0.03,  # Mono_Nonclassical
    0.08, 0.07, 0.08, 0.08,  # Macrophages
    0.06, 0.06, 0.05, 0.06,  # NK_cells
    0.04, 0.04, 0.03, 0.04,  # cDCs
    0.02, 0.02, 0.02, 0.03   # pDCs
  ), nrow = n_pops, ncol = 4, byrow = TRUE,
  dimnames = list(pop_names, c("WT_untreated", "WT_treated",
                                "KO_untreated", "KO_treated")))

  # --- Sample layout ---
  sample_info <- data.frame(
    sample_id = paste0("S", sprintf("%03d", 1:12)),
    file_name = c(
      paste0("KO_treated_rep", 1:3, ".fcs"),
      paste0("KO_untreated_rep", 1:3, ".fcs"),
      paste0("WT_treated_rep", 1:3, ".fcs"),
      paste0("WT_untreated_rep", 1:3, ".fcs")
    ),
    condition = rep(c("KO_treated", "KO_untreated", "WT_treated", "WT_untreated"),
                     each = 3),
    genotype  = rep(c("KO", "KO", "WT", "WT"), each = 3),
    treatment = rep(c("treated", "untreated", "treated", "untreated"), each = 3),
    stringsAsFactors = FALSE
  )

  # --- Helper: Dirichlet sampler ---
  rdirichlet <- function(alpha) {
    g <- rgamma(length(alpha), shape = alpha, rate = 1)
    g / sum(g)
  }

  # --- Helper: generate a population block in arcsinh space ---
  # Uses real per-marker neg/pos parameters for each expression level.
  # "lo" = 30% of way from neg to pos, "hi" = 35% beyond pos peak.
  generate_pop_arcsinh <- function(n, profile_vec) {
    mat <- matrix(0, nrow = n, ncol = n_bio)
    colnames(mat) <- bio_markers

    for (m in seq_len(n_bio)) {
      mk <- bio_markers[m]
      lev <- profile_vec[mk]
      mp <- marker_params[mk, ]

      if (lev == "neg") {
        mu <- mp$neg_mean
        s  <- mp$neg_sd
      } else if (lev == "lo") {
        # 30% of the way from negative to positive peak
        mu <- mp$neg_mean + 0.30 * (mp$pos_mean - mp$neg_mean)
        s  <- 0.5 * (mp$neg_sd + mp$pos_sd)
      } else if (lev == "pos") {
        mu <- mp$pos_mean
        s  <- mp$pos_sd
      } else {
        # "hi": beyond the positive peak
        mu <- mp$pos_mean + 0.35 * (mp$pos_mean - mp$neg_mean)
        s  <- mp$pos_sd * 1.2
      }

      mat[, m] <- rnorm(n, mean = mu, sd = s)
    }

    # Per-cell brightness offset (staining variation: all markers shift together)
    brightness <- rnorm(n, 0, 0.10)
    mat <- sweep(mat, 1, brightness, "+")

    # Within-population jitter: 10% of cells get a random marker boosted
    jitter_idx <- which(runif(n) < 0.10)
    if (length(jitter_idx) > 0) {
      for (j in jitter_idx) {
        mk_idx <- sample(n_bio, 1)
        mat[j, mk_idx] <- mat[j, mk_idx] + abs(rnorm(1, 0.8, 0.4))
      }
    }

    # Independent instrument noise per channel
    mat <- mat + matrix(rnorm(n * n_bio, 0, 0.08), nrow = n, ncol = n_bio)

    mat
  }

  # --- Helper: generate transition cells ---
  generate_transitions <- function(n, profile_from, profile_to) {
    mat_from <- generate_pop_arcsinh(n, profile_from)
    mat_to   <- generate_pop_arcsinh(n, profile_to)
    # Random blend weight per cell (beta distribution: more near edges)
    w <- rbeta(n, 2, 2)
    mat_from * (1 - w) + mat_to * w
  }

  # --- Helper: generate doublet events ---
  generate_doublets <- function(n, all_profiles) {
    mat <- matrix(0, nrow = n, ncol = n_bio)
    colnames(mat) <- bio_markers
    for (j in seq_len(n)) {
      pops <- sample(n_pops, 2)
      p1 <- setNames(all_profiles[pops[1], ], bio_markers)
      p2 <- setNames(all_profiles[pops[2], ], bio_markers)
      c1 <- generate_pop_arcsinh(1, p1)
      c2 <- generate_pop_arcsinh(1, p2)
      w <- runif(1, 0.3, 0.7)
      mat[j, ] <- w * c1 + (1 - w) * c2
    }
    # Doublets have higher scatter — add extra noise
    mat <- mat + rnorm(n * n_bio, 0, 0.2)
    mat
  }

  # --- Helper: arcsinh space -> raw fluorescence ---
  arcsinh_to_raw <- function(mat_arcsinh) {
    sinh(mat_arcsinh) * cofactor
  }

  # --- Generate FCS files ---
  for (i in seq_len(nrow(sample_info))) {
    cond <- sample_info$condition[i]
    base_props <- freq_mat[, cond]

    # Dirichlet jitter (alpha = 50 * base proportions -> ~3-8% replicate variation)
    props <- rdirichlet(50 * base_props)

    # Allocate cells: 70% pure populations, 22% transitions, 8% doublets
    n_pure   <- as.integer(round(n_cells * 0.70))
    n_trans  <- as.integer(round(n_cells * 0.22))
    n_doub   <- n_cells - n_pure - n_trans

    # Pure population cells
    n_per_pop <- as.integer(round(props * n_pure))
    diff_n <- n_pure - sum(n_per_pop)
    if (diff_n != 0) n_per_pop[which.max(n_per_pop)] <-
      n_per_pop[which.max(n_per_pop)] + diff_n

    bio_mat <- matrix(0, nrow = 0, ncol = n_bio)
    colnames(bio_mat) <- bio_markers

    for (p in seq_along(pop_names)) {
      if (n_per_pop[p] == 0) next
      profile_vec <- setNames(profile_mat[p, ], bio_markers)
      pop_cells <- generate_pop_arcsinh(n_per_pop[p], profile_vec)
      bio_mat <- rbind(bio_mat, pop_cells)
    }

    # Transition cells
    if (n_trans > 0) {
      n_per_trans <- as.integer(round(
        n_trans * sapply(transitions, `[[`, "frac") /
          sum(sapply(transitions, `[[`, "frac"))
      ))
      diff_t <- n_trans - sum(n_per_trans)
      if (diff_t != 0) n_per_trans[1] <- n_per_trans[1] + diff_t

      for (ti in seq_along(transitions)) {
        if (n_per_trans[ti] == 0) next
        tr <- transitions[[ti]]
        pf <- setNames(profile_mat[tr$from, ], bio_markers)
        pt <- setNames(profile_mat[tr$to, ], bio_markers)
        trans_cells <- generate_transitions(n_per_trans[ti], pf, pt)
        bio_mat <- rbind(bio_mat, trans_cells)
      }
    }

    # Doublet cells
    if (n_doub > 0) {
      doub_cells <- generate_doublets(n_doub, profile_mat)
      bio_mat <- rbind(bio_mat, doub_cells)
    }

    # --- Per-sample batch effect ---
    # Systematic shift per sample (instrument drift / staining variation)
    batch_shift <- rnorm(n_bio, 0, 0.12)
    bio_mat <- sweep(bio_mat, 2, batch_shift, "+")

    # --- Spectral bleed between adjacent fluorochrome channels ---
    # Small correlated noise between detector neighbours (compensation artifacts)
    bleed_pairs <- list(
      c("CD4", "CD8a"),      # BUV496 -> BUV661
      c("CD19", "Ly6G"),     # BV421 -> BV510
      c("CD11c", "Ly6C"),    # PE -> PE-CF594
      c("LAG3", "Ki67"),     # APC -> APC-R700
      c("MHCII", "CD103"),   # BUV737 -> BV605
      c("F480", "NK11")      # BV711 -> BV785
    )
    for (bp in bleed_pairs) {
      idx1 <- match(bp[1], bio_markers)
      idx2 <- match(bp[2], bio_markers)
      bleed_noise <- rnorm(nrow(bio_mat), 0, 0.15)
      bio_mat[, idx1] <- bio_mat[, idx1] + bleed_noise
      bio_mat[, idx2] <- bio_mat[, idx2] + bleed_noise * runif(1, 0.3, 0.7)
    }

    # --- Convert arcsinh -> raw fluorescence ---
    raw_bio <- arcsinh_to_raw(bio_mat)

    # --- Build full channel matrix (add scatter, time, LD, CD45) ---
    n_total <- nrow(raw_bio)
    full_mat <- matrix(0, nrow = n_total, ncol = nrow(channels))
    colnames(full_mat) <- channels$name

    # Fill bio marker columns
    for (m in seq_len(n_bio)) {
      full_mat[, bio_col_idx[m]] <- raw_bio[, m]
    }

    # Scatter channels (realistic distributions from real data)
    full_mat[, "FSC-H"] <- rlnorm(n_total, log(1300000), 0.35)
    full_mat[, "FSC-A"] <- full_mat[, "FSC-H"] * rlnorm(n_total, log(1.2), 0.15)
    full_mat[, "SSC-H"] <- rlnorm(n_total, log(280000), 0.55)
    full_mat[, "SSC-A"] <- full_mat[, "SSC-H"] * rlnorm(n_total, log(1.3), 0.20)
    full_mat[, "SSC-B-H"] <- full_mat[, "SSC-H"] * rlnorm(n_total, log(0.8), 0.20)
    full_mat[, "SSC-B-A"] <- full_mat[, "SSC-B-H"] * rlnorm(n_total, log(1.3), 0.20)
    full_mat[, "Time"] <- sort(runif(n_total, 30000, 850000))
    full_mat[, "FJComp-Zombie-NIR-A"] <- rlnorm(n_total, log(800), 0.50)
    full_mat[, "FJComp-APC-Fire810-A"] <- rlnorm(n_total, log(90000), 0.40)

    # Shuffle rows
    full_mat <- full_mat[sample(n_total), , drop = FALSE]
    # Restore time order after shuffle
    full_mat[, "Time"] <- sort(full_mat[, "Time"])
    colnames(full_mat) <- channels$name

    # Build flowFrame parameter annotation
    fcs_params <- S4Vectors::DataFrame(
      name     = channels$name,
      desc     = channels$desc,
      range    = channels$range,
      minRange = channels$minRange,
      maxRange = channels$maxRange
    )
    rownames(fcs_params) <- paste0("$P", seq_len(nrow(channels)))

    ff <- flowCore::flowFrame(
      exprs = full_mat,
      parameters = Biobase::AnnotatedDataFrame(as.data.frame(fcs_params))
    )
    flowCore::write.FCS(ff, file.path(tmp, sample_info$file_name[i]))
  }

  # --- Build metadata Excel ---
  # Sheet 1: Pipeline Settings
  settings <- data.frame(
    Variable = c(
      "clusteringMethodToUse", "markersToClusterBy", "kValuesIWant", "knn",
      "dimRedMethodToUse", "markersToDimRedBy",
      "runQC", "useQC",
      "downsampleTo", "RDataFolder", "excludeTheseSamples",
      "gimmePDFs", "greyscalePlots", "quantileNormaliseAll",
      "runInParallel", "nCores", "ramPerCore",
      "themeToUse", "viridisColour",
      "runScGate"
    ),
    Setting = c(
      "FlowSOM", "all", "10 20", "10",
      "UMAP", "all",
      "None", "FALSE",
      NA, NA, NA,
      "FALSE", "FALSE", "FALSE",
      "FALSE", "1", "4",
      "prism", "viridis",
      "FALSE"
    ),
    stringsAsFactors = FALSE
  )

  # Apply parameter overrides
  for (nm in names(params)) {
    idx <- which(settings$Variable == nm)
    if (length(idx) == 1) {
      settings$Setting[idx] <- params[[nm]]
    }
  }

  # Sheet 2: File Data
  file_data <- data.frame(
    file_name = sample_info$file_name,
    sample_id = sample_info$sample_id,
    condition = sample_info$condition,
    genotype  = sample_info$genotype,
    treatment = sample_info$treatment,
    stringsAsFactors = FALSE
  )

  # Sheet 3: Study Data
  n_rows <- max(length(bio_markers), length(excluded_markers), 4)
  study_data <- data.frame(
    `Markers to include for clustering` = c(bio_markers, rep(NA, n_rows - length(bio_markers))),
    `Marker Type` = c(
      if (is.null(marker_types)) {
        ifelse(bio_markers %in% type_markers, "type", "state")
      } else {
        stopifnot(length(marker_types) == length(bio_markers))
        marker_types
      },
      rep(NA, n_rows - length(bio_markers))
    ),
    `Markers to exclude completely` = c(excluded_markers, rep(NA, n_rows - length(excluded_markers))),
    `Cofactors for markers to use` = c(rep(2500, length(bio_markers)),
                                        rep(NA, n_rows - length(bio_markers))),
    `Conditions To Test` = c(
      "KO_treated over WT_untreated",
      "KO_untreated over WT_untreated",
      "WT_treated over WT_untreated",
      rep(NA, n_rows - 3)
    ),
    `Conditions Order` = c(
      "WT_untreated", "WT_treated", "KO_untreated", "KO_treated",
      rep(NA, n_rows - 4)
    ),
    `Cells per condition in UMAPs etc.` = c(
      rep(as.character(n_cells * 3), 4),
      rep(NA, n_rows - 4)
    ),
    `Marker Pairs` = rep(NA, n_rows),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", settings)
  openxlsx::addWorksheet(wb, "File Data")
  openxlsx::writeData(wb, "File Data", file_data)
  openxlsx::addWorksheet(wb, "Study Data")
  openxlsx::writeData(wb, "Study Data", study_data)
  meta_path <- file.path(tmp, "MARMOT_metadata.xlsx")
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)

  tmp
}

#' Skip helper for pipeline integration tests
skip_pipeline_deps <- function() {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto not installed")
  skip_if_not_installed("flowCore")
  skip_if_not_installed("FlowSOM")
  skip_if_not_installed("anndataR")
  skip_if_not_installed("pacman")
}

#' Run a pipeline integration test
#'
#' Creates synthetic data with parameter overrides, renders the pipeline,
#' and returns paths for validation.
#'
#' @param params Named list of Pipeline Settings overrides
#' @param test_name Name for the pipeline output (default "IntTest")
#' @param n_cells Cells per FCS file (default 500)
#' @param n_markers Number of markers (default 8)
#' @return A list with test_dir, results_path, h5ad_path, params, n_cells, n_markers
run_pipeline_test <- function(params = list(), test_name = "IntTest",
                              n_cells = 500, n_markers = 8,
                              marker_types = NULL) {
  test_dir <- make_test_pipeline_data(n_cells = n_cells, n_markers = n_markers,
                                       params = params, marker_types = marker_types)

  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")
  stopifnot(file.exists(meta_path))

  marmot(metadata = meta_path, name = test_name, render = TRUE)

  # Find the results directory
  results_dirs <- list.dirs(test_dir, recursive = FALSE)
  results_dir <- grep("^Results_Files_", basename(results_dirs), value = TRUE)
  stopifnot(length(results_dir) == 1)
  results_path <- file.path(test_dir, results_dir)

  h5ad_path <- file.path(results_path, "R_files", "marmot_results.h5ad")

  list(
    test_dir = test_dir,
    results_path = results_path,
    h5ad_path = h5ad_path,
    params = params,
    n_cells = n_cells,
    n_markers = n_markers
  )
}

#' Validate common pipeline output structure
#'
#' Runs structural checks that every pipeline run should pass.
#'
#' @param result Output from run_pipeline_test()
#' @param expected_cells Expected total cell count, or NULL to skip the check
validate_pipeline_output <- function(result, expected_cells = NULL) {
  h5ad_path <- result$h5ad_path

  # h5ad file exists
  expect_true(file.exists(h5ad_path))

  # Read and check manifest
  ad <- anndataR::read_h5ad(h5ad_path)
  expect_equal(ad$uns$marmot_manifest$format, "marmot-h5ad-v1")

  # Cell count
  if (!is.null(expected_cells)) {
    expect_equal(ad$n_obs(), expected_cells)
  }

  # Assays (layers + X)
  expect_true(length(ad$layers_keys()) >= 1)

  # Reduced dimensions
  expect_true(length(ad$obsm_keys()) >= 1)

  # SCE reconstructs
  sce <- reconstruct_sce_from_h5ad(h5ad_path)
  expect_s4_class(sce, "SingleCellExperiment")

  # Excel output
  excel_dir <- file.path(result$results_path, "Excel_Files")
  expect_true(dir.exists(excel_dir))
  xlsx_files <- list.files(excel_dir, pattern = "\\.xlsx$")
  expect_true(length(xlsx_files) >= 1)

  # HTML report (lives inside results dir)
  html_files <- list.files(result$results_path, pattern = "\\.html$")
  expect_true(length(html_files) >= 1)

  invisible(sce)
}

#' Run a realistic pipeline integration test (12 FCS files, 21 markers, 30 channels)
#'
#' @param params Named list of Pipeline Settings overrides
#' @param test_name Name for the pipeline output (default "RealisticTest")
#' @param n_cells Cells per FCS file (default 300)
#' @param marker_types Optional vector of 21 marker types
#' @return A list with test_dir, results_path, h5ad_path, params, n_cells, marker_types
run_realistic_pipeline_test <- function(params = list(), test_name = "RealisticTest",
                                         n_cells = 300, marker_types = NULL) {
  test_dir <- make_realistic_pipeline_data(n_cells = n_cells, params = params,
                                            marker_types = marker_types)

  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")
  stopifnot(file.exists(meta_path))

  marmot(metadata = meta_path, name = test_name, render = TRUE)

  # Find the results directory
  results_dirs <- list.dirs(test_dir, recursive = FALSE)
  results_dir <- grep("^Results_Files_", basename(results_dirs), value = TRUE)
  stopifnot(length(results_dir) == 1)
  results_path <- file.path(test_dir, results_dir)

  h5ad_path <- file.path(results_path, "R_files", "marmot_results.h5ad")

  list(
    test_dir = test_dir,
    results_path = results_path,
    h5ad_path = h5ad_path,
    params = params,
    n_cells = n_cells,
    marker_types = marker_types
  )
}

#' Validate marker type assignment in pipeline output
#'
#' Reconstructs SCE from h5ad and checks marker_class, DA results,
#' and DS results against expectations.
#'
#' @param result Output from run_pipeline_test() or run_realistic_pipeline_test()
#' @param expected_marker_classes Named vector: marker_name -> expected class ("type"/"state")
#' @param expected_n_contrasts Expected number of DA/DS contrast entries
#' @param expect_ds_markers Character vector of markers expected in DS results (NULL to skip)
#' @param expect_ds_saved Whether DS results should be saved to h5ad (default TRUE)
validate_marker_type_output <- function(result, expected_marker_classes,
                                         expected_n_contrasts = NULL,
                                         expect_ds_markers = NULL,
                                         expect_ds_saved = TRUE) {
  h5ad_path <- result$h5ad_path

  # Reconstruct SCE and check marker_class
  sce <- reconstruct_sce_from_h5ad(h5ad_path)
  rd <- SummarizedExperiment::rowData(sce)
  actual_classes <- setNames(as.character(rd$marker_class), rownames(rd))

  for (marker in names(expected_marker_classes)) {
    expect_equal(
      actual_classes[[marker]], expected_marker_classes[[marker]],
      info = paste("marker_class for", marker)
    )
  }

  # DA results (stored in uns)
  ad <- anndataR::read_h5ad(h5ad_path)
  if (!is.null(expected_n_contrasts)) {
    da_results <- ad$uns$da_results
    expect_equal(length(da_results), expected_n_contrasts,
                 info = "Number of DA contrast result entries")

    # Each DA result should have p_adj column
    for (name in names(da_results)) {
      da_df <- as.data.frame(da_results[[name]])
      expect_true("p_adj" %in% colnames(da_df), info = paste("p_adj in", name))
    }
  }

  # DS results
  if (expect_ds_saved) {
    ds_results <- ad$uns$ds_results
    expect_true(!is.null(ds_results) && length(ds_results) > 0,
                info = "DS results saved to h5ad")

    if (!is.null(expect_ds_markers)) {
      all_ds_markers <- character(0)
      for (name in names(ds_results)) {
        ds_df <- as.data.frame(ds_results[[name]])
        if ("marker_id" %in% colnames(ds_df)) {
          all_ds_markers <- union(all_ds_markers, unique(as.character(ds_df$marker_id)))
        }
      }
      for (mk in expect_ds_markers) {
        expect_true(mk %in% all_ds_markers, info = paste("DS tested marker", mk))
      }
    }
  }

  # h5ad round-trip: marker_class survives
  sce2 <- reconstruct_sce_from_h5ad(h5ad_path)
  rd2 <- SummarizedExperiment::rowData(sce2)
  expect_equal(as.character(rd2$marker_class), as.character(rd$marker_class),
               info = "marker_class survives h5ad round-trip")

  invisible(sce)
}
