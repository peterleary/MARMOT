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
make_test_pipeline_data <- function(n_cells = 500, n_markers = 8) {
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
    params <- S4Vectors::DataFrame(
      name = channel_names,
      desc = marker_names,
      range = rep(4096, n_markers),
      minRange = rep(0, n_markers),
      maxRange = rep(4096, n_markers)
    )
    rownames(params) <- paste0("$P", seq_len(n_markers))

    ff <- flowCore::flowFrame(
      exprs = mat,
      parameters = Biobase::AnnotatedDataFrame(as.data.frame(params))
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
      "themeToUse", "viridisColour"
    ),
    Setting = c(
      "FlowSOM", "all", "10", "10",
      "UMAP", "all",
      "None", "FALSE",
      NA, NA, NA,
      "FALSE", "FALSE", "FALSE",
      "FALSE", "1", "4",
      "prism", "viridis"
    ),
    stringsAsFactors = FALSE
  )

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
    `Marker Type` = c(rep("type", ceiling(n_markers / 2)),
                      rep("state", n_markers - ceiling(n_markers / 2)),
                      rep(NA, n_rows - n_markers)),
    `Markers to exclude completely` = rep(NA, n_rows),
    `Cofactors for markers to use` = c(rep(150, n_markers), rep(NA, n_rows - n_markers)),
    `Conditions To Test` = c("Treatment.vs.Control", rep(NA, n_rows - 1)),
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
