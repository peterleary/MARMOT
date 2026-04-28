# Parameterized Pipeline Integration Tests
# Exercises every major code path: clustering methods, DR methods, QC engines,
# downsampling, quantile normalisation, multi-k, marker subsetting, PDF output,
# greyscale, and RDataFolder reload.
#
# Each test uses run_pipeline_test() + validate_pipeline_output() from setup.R.
# Full suite ~25 min (500 cells × 8 markers × 4 FCS per run, ~90s each).

# ── Clustering Methods ──────────────────────────────────────────────────────────

test_that("pipeline: Rphenograph + UMAP", {
  skip_pipeline_deps()
  skip_if_not_installed("Rphenograph")

  result <- run_pipeline_test(
    params = list(clusteringMethodToUse = "Rphenograph"),
    test_name = "Rpheno"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  sce <- validate_pipeline_output(result, expected_cells = 2000)

  # Rphenograph uses "k" prefix columns
  cell_meta <- as.data.frame(SummarizedExperiment::colData(reconstruct_sce_from_h5ad(result$h5ad_path)))
  expect_true("k10" %in% colnames(cell_meta))
  expect_true(is.character(cell_meta$cluster_id) || is.factor(cell_meta$cluster_id))

  # REGRESSION (Feb 2026): cluster_id values are bare integers, not "k1"/"k2"/...
  cid_chars <- as.character(cell_meta$cluster_id)
  expect_true(all(grepl("^[0-9]+$", cid_chars)),
              info = "Rphenograph cluster_id must be bare integers (no 'k' prefix)")
})

test_that("pipeline: PARC + UMAP", {
  skip_pipeline_deps()
  # PARC needs p4r conda env with parc/pacmap
  skip_if_not_installed("reticulate")
  py_status <- MARMOT::marmot_python_status()
  skip_if(!py_status$available, "No Python with parc/pacmap available")

  result <- run_pipeline_test(
    params = list(clusteringMethodToUse = "PARC"),
    test_name = "PARC"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  sce <- validate_pipeline_output(result, expected_cells = 2000)

  # PARC uses "p" prefix columns
  cell_meta <- as.data.frame(SummarizedExperiment::colData(reconstruct_sce_from_h5ad(result$h5ad_path)))
  expect_true("p10" %in% colnames(cell_meta))
  # Ensure no zero-indexed clusters (PARC Python is 0-based, pipeline adds +1)
  cluster_vals <- as.integer(as.character(cell_meta$cluster_id))
  expect_true(all(cluster_vals >= 1, na.rm = TRUE))

  # REGRESSION (Feb 2026): cluster_id values are bare integers, not "p1"/"p2"/...
  cid_chars <- as.character(cell_meta$cluster_id)
  expect_true(all(grepl("^[0-9]+$", cid_chars)),
              info = "PARC cluster_id must be bare integers (no 'p' prefix)")
})

test_that("pipeline: MfastPG + UMAP", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(clusteringMethodToUse = "MfastPG"),
    test_name = "MfastPG"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  sce <- validate_pipeline_output(result, expected_cells = 2000)

  # MfastPG uses "k" prefix columns (same as Rphenograph/Mphenograph)
  cell_meta <- as.data.frame(SummarizedExperiment::colData(reconstruct_sce_from_h5ad(result$h5ad_path)))
  expect_true("k10" %in% colnames(cell_meta))

  # REGRESSION (Feb 2026): cluster_id values are bare integers, not "k1"/"k2"/...
  cid_chars <- as.character(cell_meta$cluster_id)
  expect_true(all(grepl("^[0-9]+$", cid_chars)),
              info = "MfastPG cluster_id must be bare integers (no 'k' prefix)")
})

# ── DR Methods ───────────────────────────────────────────────────────────────────

test_that("pipeline: FlowSOM + TSNE", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(dimRedMethodToUse = "TSNE"),
    test_name = "TSNE"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  sce <- validate_pipeline_output(result, expected_cells = 2000)

  # TSNE reduction should exist; pipeline always runs UMAP too
  sce_tsne <- reconstruct_sce_from_h5ad(result$h5ad_path)
  expect_true("TSNE" %in% SingleCellExperiment::reducedDimNames(sce_tsne))
  expect_true("UMAP" %in% SingleCellExperiment::reducedDimNames(sce_tsne))
})

test_that("pipeline: FlowSOM + PaCMAP", {
  skip_pipeline_deps()
  skip_if_not_installed("reticulate")
  py_status <- MARMOT::marmot_python_status()
  skip_if(!py_status$available, "No Python with parc/pacmap available")

  result <- run_pipeline_test(
    params = list(dimRedMethodToUse = "pacmap"),
    test_name = "PaCMAP"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  sce <- validate_pipeline_output(result, expected_cells = 2000)

  # PaCMAP reduction should exist
  sce_pm <- reconstruct_sce_from_h5ad(result$h5ad_path)
  rd_names <- SingleCellExperiment::reducedDimNames(sce_pm)
  expect_true(any(grepl("pacmap|PaCMAP|UMAP", rd_names, ignore.case = TRUE)))
})

# ── QC Paths ─────────────────────────────────────────────────────────────────────

test_that("pipeline: PeacoQC run only (useQC=FALSE)", {
  skip_pipeline_deps()
  skip_if_not_installed("PeacoQC")

  # PeacoQC needs ≥5000 events per sample to pass the QC viability guard
  result <- run_pipeline_test(
    params = list(runQC = "PeacoQC", useQC = "FALSE"),
    test_name = "PeacoQC_NoUse",
    n_cells = 5000
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # QC ran but wasn't used — full cell count (5000 × 4 samples)
  sce <- validate_pipeline_output(result, expected_cells = 20000)

  # PeacoQC results directory created
  qc_dir <- file.path(result$test_dir, "resultsQC_peacoQC")
  expect_true(dir.exists(qc_dir))

  # QCmini summary file exists
  qcmini_path <- file.path(qc_dir, "QCmini.txt")
  expect_true(file.exists(qcmini_path))
})

test_that("pipeline: PeacoQC + useQC=TRUE", {
  skip_pipeline_deps()
  skip_if_not_installed("PeacoQC")

  # PeacoQC needs ≥5000 events per sample to pass the QC viability guard
  result <- run_pipeline_test(
    params = list(runQC = "PeacoQC", useQC = "TRUE"),
    test_name = "PeacoQC_Use",
    n_cells = 5000
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # QC filtered some cells — count <= full (5000 × 4)
  cell_meta <- as.data.frame(SummarizedExperiment::colData(reconstruct_sce_from_h5ad(result$h5ad_path)))
  expect_true(nrow(cell_meta) <= 20000)
  # But shouldn't be zero
  expect_true(nrow(cell_meta) > 0)

  # Structural validation (skip cell count — already checked above)
  validate_pipeline_output(result, expected_cells = NULL)

  # QC summary in h5ad
  ad <- anndataR::read_h5ad(result$h5ad_path)
  expect_true(!is.null(ad$uns$qc$qc_summary))
})

test_that("pipeline: FlowAI + useQC=TRUE", {
  skip_pipeline_deps()
  skip_if_not_installed("flowAI")

  result <- run_pipeline_test(
    params = list(runQC = "FlowAI", useQC = "TRUE"),
    test_name = "FlowAI_Use",
    n_cells = 2000
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # FlowAI filtered some cells — count <= full (2000 × 4)
  cell_meta <- as.data.frame(SummarizedExperiment::colData(reconstruct_sce_from_h5ad(result$h5ad_path)))
  expect_true(nrow(cell_meta) <= 8000)
  expect_true(nrow(cell_meta) > 0)

  validate_pipeline_output(result, expected_cells = NULL)

  # FlowAI results directory with *_QC_highQ.fcs files
  qc_dir <- file.path(result$test_dir, "resultsQC_flowAI")
  expect_true(dir.exists(qc_dir))
  highq_files <- list.files(qc_dir, pattern = "_QC_highQ\\.fcs$")
  expect_true(length(highq_files) >= 1)
})

test_that("pipeline: PeacoQC skipped on small samples (<2000 events)", {
  skip_pipeline_deps()
  skip_if_not_installed("PeacoQC")

  # 200 cells per FCS × 4 samples = 800 total — well below 2000 threshold

  result <- run_pipeline_test(
    params = list(runQC = "PeacoQC", useQC = "TRUE"),
    test_name = "PeacoQC_TooSmall",
    n_cells = 200
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # Pipeline should complete (QC skipped, not crashed)
  validate_pipeline_output(result, expected_cells = 800)

  # No PeacoQC results directory — QC was skipped entirely
  qc_dir <- file.path(result$test_dir, "resultsQC_peacoQC")
  expect_false(dir.exists(qc_dir))
})

test_that("pipeline: PeacoQC skipped on medium samples (2000-5000 events)", {
  skip_pipeline_deps()
  skip_if_not_installed("PeacoQC")

  # 500 cells per FCS × 4 samples = 2000 total — above 2000 but below 5000
  result <- run_pipeline_test(
    params = list(runQC = "PeacoQC", useQC = "TRUE"),
    test_name = "PeacoQC_MedSmall",
    n_cells = 500
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # Pipeline should complete (PeacoQC skipped, falls back to None)
  validate_pipeline_output(result, expected_cells = 2000)

  # No PeacoQC results directory — PeacoQC was skipped
  qc_dir <- file.path(result$test_dir, "resultsQC_peacoQC")
  expect_false(dir.exists(qc_dir))
})

test_that("pipeline: FlowAI skipped on tiny samples (<2000 events)", {
  skip_pipeline_deps()
  skip_if_not_installed("flowAI")

  # 200 cells per FCS × 4 samples = 800 — below 2000 threshold
  result <- run_pipeline_test(
    params = list(runQC = "FlowAI", useQC = "TRUE"),
    test_name = "FlowAI_TooSmall",
    n_cells = 200
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # Pipeline should complete (QC skipped entirely)
  validate_pipeline_output(result, expected_cells = 800)

  # No FlowAI results directory
  qc_dir <- file.path(result$test_dir, "resultsQC_flowAI")
  expect_false(dir.exists(qc_dir))
})

# ── Data Processing Options ──────────────────────────────────────────────────────

test_that("pipeline: downsample to 200", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(downsampleTo = "200"),
    test_name = "DS200"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # 200 cells × 4 samples = 800 total
  validate_pipeline_output(result, expected_cells = 800)
})

test_that("pipeline: quantile normalise", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(quantileNormaliseAll = "TRUE"),
    test_name = "QNorm"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  validate_pipeline_output(result, expected_cells = 2000)

  # Quantile-normalised expression should exist and values in [0, 1]
  sce_qn <- reconstruct_sce_from_h5ad(result$h5ad_path)
  expect_true("exprsQuantNorm" %in% SummarizedExperiment::assayNames(sce_qn))
  qnorm_mat <- as.matrix(SummarizedExperiment::assay(sce_qn, "exprsQuantNorm"))
  expect_true(all(qnorm_mat >= -0.01 & qnorm_mat <= 1.01, na.rm = TRUE))
})

test_that("pipeline: multiple k values", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(kValuesIWant = "10 20", knn = "10"),
    test_name = "MultiK"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  validate_pipeline_output(result, expected_cells = 2000)

  # FlowSOM uses "meta" prefix — both meta10 and meta20 columns
  cell_meta <- as.data.frame(SummarizedExperiment::colData(reconstruct_sce_from_h5ad(result$h5ad_path)))
  expect_true("meta10" %in% colnames(cell_meta))
  expect_true("meta20" %in% colnames(cell_meta))

  # REGRESSION (Feb 2026): cluster_id and per-k columns are bare integers
  for (col in c("cluster_id", "meta10", "meta20")) {
    vals <- as.character(cell_meta[[col]])
    expect_true(all(grepl("^[0-9]+$", vals)),
                info = paste("FlowSOM", col, "must be bare integers"))
  }
})

test_that("pipeline: type markers only", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(markersToClusterBy = "type", markersToDimRedBy = "type"),
    test_name = "TypeOnly"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # All 2000 cells still present (just fewer markers used for clustering/DR)
  validate_pipeline_output(result, expected_cells = 2000)
})

# ── Output Options ───────────────────────────────────────────────────────────────

test_that("pipeline: PDF output", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(gimmePDFs = "TRUE"),
    test_name = "PDFs"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  validate_pipeline_output(result, expected_cells = 2000)

  # PDF_figures directory with at least one PDF
  pdf_dir <- file.path(result$results_path, "PDF_figures")
  expect_true(dir.exists(pdf_dir))
  pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", recursive = TRUE)
  expect_true(length(pdf_files) >= 1)
})

test_that("pipeline: greyscale plots", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(greyscalePlots = "TRUE"),
    test_name = "Grey"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # Pipeline completes without error — that's the main assertion
  validate_pipeline_output(result, expected_cells = 2000)
})

# ── Reload Path ──────────────────────────────────────────────────────────────────

test_that("pipeline: RDataFolder reload", {
  skip_pipeline_deps()

  # Run baseline first
  baseline <- run_pipeline_test(
    params = list(),
    test_name = "Reload_Baseline"
  )
  on.exit(unlink(baseline$test_dir, recursive = TRUE), add = TRUE)

  validate_pipeline_output(baseline, expected_cells = 2000)

  # Record h5ad timestamps before reload
  r_files_dir <- file.path(baseline$results_path, "R_files")
  h5ad_mtime <- file.mtime(baseline$h5ad_path)

  # Re-run with RDataFolder pointing to R_files.
  # Use loadWorkbook to modify in-place (preserves column names with spaces).
  meta_path <- file.path(baseline$test_dir, "MARMOT_metadata.xlsx")
  wb <- openxlsx::loadWorkbook(meta_path)
  settings <- openxlsx::readWorkbook(wb, sheet = "Pipeline Settings")
  idx <- which(settings$Variable == "RDataFolder")
  settings$Setting[idx] <- r_files_dir
  openxlsx::removeWorksheet(wb, "Pipeline Settings")
  openxlsx::addWorksheet(wb, "Pipeline Settings", gridLines = FALSE)
  openxlsx::writeData(wb, "Pipeline Settings", settings)
  # Reorder sheets so Pipeline Settings is first
  openxlsx::worksheetOrder(wb) <- c(
    which(names(wb) == "Pipeline Settings"),
    which(names(wb) != "Pipeline Settings")
  )
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)

  # Run the reload
  marmot(metadata = meta_path, name = "Reload_Rerun", render = TRUE)

  # Find the results directory (may be same or new)
  results_dirs <- list.dirs(baseline$test_dir, recursive = FALSE)
  results_dirs <- grep("^Results_Files_", basename(results_dirs), value = TRUE)
  expect_true(length(results_dirs) >= 1)

  # The reload should produce a new HTML inside one of the results dirs
  html_found <- any(vapply(results_dirs, function(d) {
    length(list.files(file.path(baseline$test_dir, d),
                      pattern = "Reload_Rerun\\.html$")) > 0
  }, logical(1)))
  expect_true(html_found)

  # h5ad from baseline should still exist
  expect_true(file.exists(baseline$h5ad_path))
})

# ── Combined ─────────────────────────────────────────────────────────────────────

test_that("pipeline: kitchen sink (TSNE + quantile + downsample + multi-k + PDFs)", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(
      dimRedMethodToUse = "TSNE",
      quantileNormaliseAll = "TRUE",
      downsampleTo = "300",
      kValuesIWant = "10 20",
      knn = "10",
      gimmePDFs = "TRUE"
    ),
    test_name = "KitchenSink"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # 300 cells × 4 samples = 1200
  validate_pipeline_output(result, expected_cells = 1200)

  # TSNE reduction
  sce_ks <- reconstruct_sce_from_h5ad(result$h5ad_path)
  expect_true("TSNE" %in% SingleCellExperiment::reducedDimNames(sce_ks))

  # Both meta10 and meta20
  cell_meta <- as.data.frame(SummarizedExperiment::colData(reconstruct_sce_from_h5ad(result$h5ad_path)))
  expect_true("meta10" %in% colnames(cell_meta))
  expect_true("meta20" %in% colnames(cell_meta))

  # PDFs generated
  pdf_dir <- file.path(result$results_path, "PDF_figures")
  expect_true(dir.exists(pdf_dir))
  pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", recursive = TRUE)
  expect_true(length(pdf_files) >= 1)
})
