# Tests for R/parquet_io.R — Parquet round-trip
# Suppress tibble row names deprecation warnings from arrow::read_parquet
withr::local_options(list(lifecycle_verbosity = "quiet"), .local_envir = teardown_env())

test_that("save_parquet_data creates expected directory structure", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)

  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  env$smd <- data.frame(study = "test", stringsAsFactors = FALSE)
  env$umapDFList <- list(All = umap_df)
  env$coloursList <- colours

  save_parquet_data(tmp, envir = env)

  pq_dir <- file.path(tmp, "parquet")
  expect_true(dir.exists(pq_dir))
  expect_true(file.exists(file.path(pq_dir, "_manifest.json")))
  expect_true(dir.exists(file.path(pq_dir, "expression")))
  expect_true(dir.exists(file.path(pq_dir, "reductions")))
  expect_true(dir.exists(file.path(pq_dir, "dr_dataframes")))
  expect_true(dir.exists(file.path(pq_dir, "colours")))

  manifest <- jsonlite::fromJSON(file.path(pq_dir, "_manifest.json"))
  expect_equal(manifest$format, "marmot-parquet-v1")
})

test_that("round-trip: metadata survives save/load", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$smd <- data.frame(study = "test", stringsAsFactors = FALSE)
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- colours

  save_parquet_data(tmp, envir = env_save)
  pq_dir <- file.path(tmp, "parquet")

  env_load <- new.env(parent = emptyenv())
  load_parquet_to_env(pq_dir, envir = env_load)

  expect_true(exists("md", envir = env_load))
  loaded_md <- get("md", envir = env_load)
  expect_equal(nrow(loaded_md), nrow(env_save$md))
  expect_equal(sort(colnames(loaded_md)), sort(colnames(env_save$md)))
})

test_that("round-trip: SCE dimensions and assays survive", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- make_mock_colours(sce)

  save_parquet_data(tmp, envir = env_save)
  pq_dir <- file.path(tmp, "parquet")

  sce2 <- reconstruct_sce_from_parquet(pq_dir)
  expect_s4_class(sce2, "SingleCellExperiment")
  expect_equal(ncol(sce2), ncol(sce))
  expect_equal(nrow(sce2), nrow(sce))
  expect_equal(sort(SummarizedExperiment::assayNames(sce2)),
               sort(SummarizedExperiment::assayNames(sce)))
  expect_equal(sort(colnames(SummarizedExperiment::colData(sce2))),
               sort(colnames(SummarizedExperiment::colData(sce))))

  # Expression values match within tolerance
  orig <- as.matrix(SummarizedExperiment::assay(sce, "exprsTransformed"))
  loaded <- as.matrix(SummarizedExperiment::assay(sce2, "exprsTransformed"))
  # Align by cell_id (column names)
  common_cells <- intersect(colnames(orig), colnames(loaded))
  expect_equal(orig[, common_cells], loaded[, common_cells], tolerance = 1e-6)
})

test_that("round-trip: umapDFList names and dimensions survive", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- make_mock_colours(sce)

  save_parquet_data(tmp, envir = env_save)
  pq_dir <- file.path(tmp, "parquet")

  env_load <- new.env(parent = emptyenv())
  load_parquet_to_env(pq_dir, envir = env_load)

  loaded_list <- get("umapDFList", envir = env_load)
  expect_true("All" %in% names(loaded_list))
  expect_equal(nrow(loaded_list[["All"]]), nrow(umap_df))
  expect_equal(sort(colnames(loaded_list[["All"]])), sort(colnames(umap_df)))
})

test_that("round-trip: coloursList values survive", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- colours

  save_parquet_data(tmp, envir = env_save)
  pq_dir <- file.path(tmp, "parquet")

  env_load <- new.env(parent = emptyenv())
  load_parquet_to_env(pq_dir, envir = env_load)

  loaded_colours <- get("coloursList", envir = env_load)
  expect_true("cluster.id" %in% names(loaded_colours) || "cluster_id" %in% names(loaded_colours))

  # Get the cluster colours regardless of dot/underscore naming
  cluster_key <- intersect(c("cluster_id", "cluster.id"), names(loaded_colours))
  expect_length(cluster_key, 1)
  expect_equal(sort(unname(loaded_colours[[cluster_key]])),
               sort(unname(colours$cluster_id)))
})

test_that("load_parquet_for_shiny returns expected keys", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$smd <- data.frame(study = "test", stringsAsFactors = FALSE)
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- colours

  save_parquet_data(tmp, envir = env_save)
  pq_dir <- file.path(tmp, "parquet")

  result <- load_parquet_for_shiny(pq_dir)
  expect_true(is.list(result))
  expect_true("sce" %in% names(result))
  expect_true("md" %in% names(result))
  expect_true("smd" %in% names(result))
  expect_true("umapDFList" %in% names(result))
  expect_true("coloursList" %in% names(result))
  expect_true("conditions" %in% names(result))
  expect_s4_class(result$sce, "SingleCellExperiment")
})

test_that("save_parquet_data handles empty coloursList entries without crashing", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)

  # Include an empty named character vector — this used to crash
  colours <- make_mock_colours(sce)
  colours$empty_entry <- setNames(character(0), character(0))

  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  env$umapDFList <- list(All = umap_df)
  env$coloursList <- colours

  expect_no_error(save_parquet_data(tmp, envir = env))

  # The empty entry should be skipped, non-empty entries saved
  col_files <- list.files(file.path(tmp, "parquet", "colours"), pattern = "\\.parquet$")
  expect_true(length(col_files) >= 2)  # cluster_id + condition, not empty_entry
})
