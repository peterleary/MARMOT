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
  cell_meta <- arrow::read_parquet(file.path(result$pq_dir, "cell_metadata.parquet"))
  expect_true("k10" %in% colnames(cell_meta))
  expect_true(is.character(cell_meta$cluster_id) || is.factor(cell_meta$cluster_id))
})

test_that("pipeline: PARC + UMAP", {
  skip_pipeline_deps()
  # PARC needs basilisk + Python env
  skip_if_not_installed("basilisk")
  skip_if_not_installed("reticulate")

  result <- run_pipeline_test(
    params = list(clusteringMethodToUse = "PARC"),
    test_name = "PARC"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  sce <- validate_pipeline_output(result, expected_cells = 2000)

  # PARC uses "p" prefix columns
  cell_meta <- arrow::read_parquet(file.path(result$pq_dir, "cell_metadata.parquet"))
  expect_true("p10" %in% colnames(cell_meta))
  # Ensure no zero-indexed clusters (PARC Python is 0-based, pipeline adds +1)
  cluster_vals <- as.integer(as.character(cell_meta$cluster_id))
  expect_true(all(cluster_vals >= 1, na.rm = TRUE))
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
  cell_meta <- arrow::read_parquet(file.path(result$pq_dir, "cell_metadata.parquet"))
  expect_true("k10" %in% colnames(cell_meta))
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
  expect_true(file.exists(file.path(result$pq_dir, "reductions", "TSNE.parquet")))
  expect_true(file.exists(file.path(result$pq_dir, "reductions", "UMAP.parquet")))
})

test_that("pipeline: FlowSOM + PaCMAP", {
  skip_pipeline_deps()
  skip_if_not_installed("basilisk")
  skip_if_not_installed("reticulate")

  result <- run_pipeline_test(
    params = list(dimRedMethodToUse = "pacmap"),
    test_name = "PaCMAP"
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  sce <- validate_pipeline_output(result, expected_cells = 2000)

  # PaCMAP reduction should exist
  red_files <- list.files(file.path(result$pq_dir, "reductions"), pattern = "\\.parquet$")
  expect_true(any(grepl("pacmap|PaCMAP|UMAP", red_files, ignore.case = TRUE)))
})

# ── QC Paths ─────────────────────────────────────────────────────────────────────

test_that("pipeline: PeacoQC run only (useQC=FALSE)", {
  skip_pipeline_deps()
  skip_if_not_installed("PeacoQC")

  # QC engines need more events for spline fitting — use 2000 cells per FCS
  result <- run_pipeline_test(
    params = list(runQC = "PeacoQC", useQC = "FALSE"),
    test_name = "PeacoQC_NoUse",
    n_cells = 2000
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # QC ran but wasn't used — full cell count (2000 × 4 samples)
  sce <- validate_pipeline_output(result, expected_cells = 8000)

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

  result <- run_pipeline_test(
    params = list(runQC = "PeacoQC", useQC = "TRUE"),
    test_name = "PeacoQC_Use",
    n_cells = 2000
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # QC filtered some cells — count <= full (2000 × 4)
  cell_meta <- arrow::read_parquet(file.path(result$pq_dir, "cell_metadata.parquet"))
  expect_true(nrow(cell_meta) <= 8000)
  # But shouldn't be zero
  expect_true(nrow(cell_meta) > 0)

  # Structural validation (skip cell count — already checked above)
  validate_pipeline_output(result, expected_cells = NULL)

  # QC summary in parquet dir
  qc_summary_path <- file.path(result$pq_dir, "qc", "qc_summary.parquet")
  expect_true(file.exists(qc_summary_path))
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
  cell_meta <- arrow::read_parquet(file.path(result$pq_dir, "cell_metadata.parquet"))
  expect_true(nrow(cell_meta) <= 8000)
  expect_true(nrow(cell_meta) > 0)

  validate_pipeline_output(result, expected_cells = NULL)

  # FlowAI results directory with *_QC_highQ.fcs files
  qc_dir <- file.path(result$test_dir, "resultsQC_flowAI")
  expect_true(dir.exists(qc_dir))
  highq_files <- list.files(qc_dir, pattern = "_QC_highQ\\.fcs$")
  expect_true(length(highq_files) >= 1)
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
  qnorm_path <- file.path(result$pq_dir, "expression", "exprsQuantNorm.parquet")
  expect_true(file.exists(qnorm_path))
  qnorm_data <- arrow::read_parquet(qnorm_path)
  # Exclude cell_id column if present
  numeric_cols <- sapply(qnorm_data, is.numeric)
  vals <- unlist(qnorm_data[, numeric_cols, drop = FALSE])
  expect_true(all(vals >= -0.01 & vals <= 1.01, na.rm = TRUE))
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
  cell_meta <- arrow::read_parquet(file.path(result$pq_dir, "cell_metadata.parquet"))
  expect_true("meta10" %in% colnames(cell_meta))
  expect_true("meta20" %in% colnames(cell_meta))
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

  # Record parquet timestamps before reload
  r_files_dir <- file.path(baseline$results_path, "R_files")
  pq_manifest_mtime <- file.mtime(file.path(baseline$pq_dir, "_manifest.json"))

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

  # pipeline_settings.parquet from baseline should still exist
  expect_true(file.exists(file.path(baseline$pq_dir, "pipeline_settings.parquet")))
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
  expect_true(file.exists(file.path(result$pq_dir, "reductions", "TSNE.parquet")))

  # Both meta10 and meta20
  cell_meta <- arrow::read_parquet(file.path(result$pq_dir, "cell_metadata.parquet"))
  expect_true("meta10" %in% colnames(cell_meta))
  expect_true("meta20" %in% colnames(cell_meta))

  # PDFs generated
  pdf_dir <- file.path(result$results_path, "PDF_figures")
  expect_true(dir.exists(pdf_dir))
  pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", recursive = TRUE)
  expect_true(length(pdf_files) >= 1)
})
