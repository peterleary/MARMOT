# Pipeline Integration Test
# End-to-end: synthetic FCS + metadata → marmot(render=TRUE) → validate outputs

test_that("pipeline integration: FlowSOM + UMAP end-to-end", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto not installed")
  skip_if_not_installed("flowCore")
  skip_if_not_installed("FlowSOM")
  skip_if_not_installed("arrow")
  skip_if_not_installed("pacman")

  # Create synthetic data
  test_dir <- make_test_pipeline_data(n_cells = 500, n_markers = 8)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")
  expect_true(file.exists(meta_path))

  # Run the pipeline
  marmot(metadata = meta_path, name = "IntTest", render = TRUE)

  # Find the results directory
  results_dirs <- list.dirs(test_dir, recursive = FALSE)
  results_dir <- grep("^Results_Files_", basename(results_dirs), value = TRUE)
  expect_length(results_dir, 1)
  results_path <- file.path(test_dir, results_dir)

  # ── Parquet output validation ──
  pq_dir <- file.path(results_path, "R_files", "parquet")
  expect_true(dir.exists(pq_dir))

  # Manifest
  manifest_path <- file.path(pq_dir, "_manifest.json")
  expect_true(file.exists(manifest_path))
  manifest <- jsonlite::fromJSON(manifest_path)
  expect_equal(manifest$format, "marmot-parquet-v1")

  # Cell metadata
  cell_meta_path <- file.path(pq_dir, "cell_metadata.parquet")
  expect_true(file.exists(cell_meta_path))
  cell_meta <- arrow::read_parquet(cell_meta_path)
  expect_equal(nrow(cell_meta), 4 * 500)  # 4 samples × 500 cells

  # Expression assays
  expr_dir <- file.path(pq_dir, "expression")
  expect_true(dir.exists(expr_dir))
  expr_files <- list.files(expr_dir, pattern = "\\.parquet$")
  expect_true(length(expr_files) >= 1)

  # UMAP reduction
  umap_path <- file.path(pq_dir, "reductions", "UMAP.parquet")
  expect_true(file.exists(umap_path))
  umap_data <- arrow::read_parquet(umap_path)
  expect_equal(ncol(umap_data), 3)  # cell_id + 2 UMAP dims
  expect_equal(nrow(umap_data), 4 * 500)

  # DR data frames
  dr_dir <- file.path(pq_dir, "dr_dataframes")
  expect_true(dir.exists(dr_dir))
  dr_files <- list.files(dr_dir, pattern = "\\.parquet$")
  expect_true(length(dr_files) >= 1)

  # ── Reconstruct SCE from Parquet ──
  sce <- reconstruct_sce_from_parquet(pq_dir)
  expect_s4_class(sce, "SingleCellExperiment")
  expect_equal(nrow(sce), 8)         # 8 markers
  expect_equal(ncol(sce), 4 * 500)   # 2000 cells

  # ── Excel output ──
  excel_dir <- file.path(results_path, "Excel_Files")
  expect_true(dir.exists(excel_dir))
  xlsx_files <- list.files(excel_dir, pattern = "\\.xlsx$")
  expect_true(length(xlsx_files) >= 1)
})
