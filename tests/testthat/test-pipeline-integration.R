# Pipeline Integration Test
# End-to-end: synthetic FCS + metadata → marmot(render=TRUE) → validate outputs

test_that("pipeline integration: FlowSOM + UMAP end-to-end", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(),
    test_name = "IntTest",
    n_cells = 500,
    n_markers = 8
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # Common structural validation
  sce <- validate_pipeline_output(result, expected_cells = 2000)

  # Specific SCE dimension checks for baseline
  expect_equal(nrow(sce), 8)         # 8 markers
  expect_equal(ncol(sce), 2000)      # 4 samples × 500 cells

  # UMAP reduction shape
  umap_data <- arrow::read_parquet(file.path(result$pq_dir, "reductions", "UMAP.parquet"))
  expect_equal(ncol(umap_data), 3)   # cell_id + 2 UMAP dims
  expect_equal(nrow(umap_data), 2000)
})
