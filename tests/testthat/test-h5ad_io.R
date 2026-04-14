# Tests for R/h5ad_io.R — h5ad round-trip

test_that("save_h5ad_data creates expected file", {
  skip_if_not_installed("anndataR")
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

  save_h5ad_data(tmp, envir = env)

  h5ad_path <- file.path(tmp, "marmot_results.h5ad")
  expect_true(file.exists(h5ad_path))

  # Check manifest via uns
  ad <- anndataR::read_h5ad(h5ad_path)
  expect_equal(ad$uns$marmot_manifest$format, "marmot-h5ad-v1")
})

test_that("round-trip: metadata survives save/load", {
  skip_if_not_installed("anndataR")
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

  save_h5ad_data(tmp, envir = env_save)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  env_load <- new.env(parent = emptyenv())
  load_h5ad_to_env(h5ad_path, envir = env_load)

  expect_true(exists("md", envir = env_load))
  loaded_md <- get("md", envir = env_load)
  expect_equal(nrow(loaded_md), nrow(env_save$md))
  expect_equal(sort(colnames(loaded_md)), sort(colnames(env_save$md)))
})

test_that("round-trip: SCE dimensions and assays survive", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- make_mock_colours(sce)

  save_h5ad_data(tmp, envir = env_save)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  sce2 <- reconstruct_sce_from_h5ad(h5ad_path)
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
  common_cells <- intersect(colnames(orig), colnames(loaded))
  expect_equal(orig[, common_cells], loaded[, common_cells], tolerance = 1e-6)
})

test_that("round-trip: umapDFList names and dimensions survive", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- make_mock_colours(sce)

  save_h5ad_data(tmp, envir = env_save)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  env_load <- new.env(parent = emptyenv())
  load_h5ad_to_env(h5ad_path, envir = env_load)

  loaded_list <- get("umapDFList", envir = env_load)
  expect_true("All" %in% names(loaded_list))
  expect_equal(nrow(loaded_list[["All"]]), nrow(umap_df))
  expect_equal(sort(colnames(loaded_list[["All"]])), sort(colnames(umap_df)))
})

test_that("round-trip: coloursList values survive", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- colours

  save_h5ad_data(tmp, envir = env_save)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  env_load <- new.env(parent = emptyenv())
  load_h5ad_to_env(h5ad_path, envir = env_load)

  loaded_colours <- get("coloursList", envir = env_load)
  expect_true("cluster_id" %in% names(loaded_colours))
  expect_equal(sort(unname(loaded_colours$cluster_id)),
               sort(unname(colours$cluster_id)))
})

test_that("load_h5ad_for_shiny returns expected keys", {
  skip_if_not_installed("anndataR")
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

  save_h5ad_data(tmp, envir = env_save)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  result <- load_h5ad_for_shiny(h5ad_path)
  expect_true(is.list(result))
  expect_true("sce" %in% names(result))
  expect_true("md" %in% names(result))
  expect_true("smd" %in% names(result))
  expect_true("umapDFList" %in% names(result))
  expect_true("coloursList" %in% names(result))
  expect_true("conditions" %in% names(result))
  expect_s4_class(result$sce, "SingleCellExperiment")
})

test_that("save_h5ad_data handles empty coloursList entries without crashing", {
  skip_if_not_installed("anndataR")
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

  expect_no_error(save_h5ad_data(tmp, envir = env))

  # The empty entry should be skipped, non-empty entries saved
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")
  ad <- anndataR::read_h5ad(h5ad_path)
  colour_keys <- names(ad$uns$colours)
  expect_true(length(colour_keys) >= 2)  # cluster_id + condition, not empty_entry
  expect_false("empty_entry" %in% colour_keys)
})
