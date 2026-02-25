# Tests for scGate-related helpers and Parquet round-trip

# ── parse_marker_pairs ──────────────────────────────────────────────────────

test_that("parse_marker_pairs extracts types and models", {
  smd <- data.frame(
    `Marker Type` = c("type", "type", "state", "state"),
    `Marker Pairs` = c("Th: CD4 CD3", "NK: CD56 CD16", NA, NA),
    check.names = FALSE
  )
  result <- parse_marker_pairs(smd)

  expect_type(result, "list")
  expect_equal(result$types, c("Th", "NK"))
  expect_equal(result$models$Th, c("CD4", "CD3"))
  expect_equal(result$models$NK, c("CD56", "CD16"))
  expect_equal(length(result$pairs), 2)
})

test_that("parse_marker_pairs returns NULL when no Marker Pairs column", {
  smd <- data.frame(`Marker Type` = c("type", "state"), check.names = FALSE)
  expect_null(parse_marker_pairs(smd))
})

test_that("parse_marker_pairs returns NULL when all NA", {
  smd <- data.frame(
    `Marker Pairs` = c(NA, NA, NA),
    check.names = FALSE
  )
  expect_null(parse_marker_pairs(smd))
})

test_that("parse_marker_pairs handles single entry", {
  smd <- data.frame(
    `Marker Pairs` = c("Treg: FOXP3 CD25", NA),
    check.names = FALSE
  )
  result <- parse_marker_pairs(smd)

  expect_equal(result$types, "Treg")
  expect_equal(result$models$Treg, c("FOXP3", "CD25"))
  expect_equal(length(result$pairs), 1)
})

test_that("parse_marker_pairs handles duplicate types (multiple pairs same type)", {
  smd <- data.frame(
    `Marker Pairs` = c("T: CD4 CD3", "T: CD8 CD3", NA),
    check.names = FALSE
  )
  result <- parse_marker_pairs(smd)

  # Only one type "T", model uses first entry
  expect_equal(result$types, "T")
  expect_equal(length(result$models), 1)
  expect_equal(result$models$T, c("CD4", "CD3"))
  # But pairs has both entries
  expect_equal(length(result$pairs), 2)
})

# ── setup_scgate_colours ────────────────────────────────────────────────────

test_that("setup_scgate_colours builds Gated_Cells colours", {
  palette <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00")
  gated <- c("Th", "NK", "Treg")
  models <- c("Th", "NK", "Treg")

  result <- setup_scgate_colours(gated, models, palette)

  expect_true("Gated_Cells" %in% names(result))
  expect_equal(length(result$Gated_Cells), 3)
  expect_equal(names(result$Gated_Cells), c("Th", "NK", "Treg"))
  expect_equal(unname(result$Gated_Cells), palette[1:3])
})

test_that("setup_scgate_colours builds is_* colours", {
  palette <- c("#FF0000", "#00FF00")
  result <- setup_scgate_colours("Th", c("Th cells", "NK-cells"), palette)

  expect_true("is_Thcells" %in% names(result))
  expect_true("is_NKcells" %in% names(result))
  expect_equal(names(result$is_Thcells), "Th cells")
  expect_equal(names(result$is_NKcells), "NK-cells")
  expect_equal(unname(result$is_Thcells), "steelblue")
})

# ── Parquet round-trip with scGate columns ──────────────────────────────────

test_that("Parquet round-trip preserves scGate colData columns", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  sce <- make_mock_sce()

  # Add scGate-like columns to colData
  n <- ncol(sce)
  sce$Gated_Cells <- sample(c("Th", "NK", "Treg"), n, replace = TRUE)
  sce$is_Th <- ifelse(sce$Gated_Cells == "Th", "Th", NA)
  sce$is_NK <- ifelse(sce$Gated_Cells == "NK", "NK", NA)
  sce$Th_UCell <- runif(n, 0, 1)
  sce$NK_UCell <- runif(n, 0, 1)

  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)
  colours$Gated_Cells <- c(Th = "#FF0000", NK = "#00FF00", Treg = "#0000FF")
  colours$is_Th <- c(Th = "steelblue")

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$smd <- data.frame(study = "test", stringsAsFactors = FALSE)
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- colours

  save_parquet_data(tmp, envir = env_save)

  # Load and verify
  pq_dir <- file.path(tmp, "parquet")
  sce2 <- reconstruct_sce_from_parquet(pq_dir)

  expect_true("Gated_Cells" %in% colnames(SummarizedExperiment::colData(sce2)))
  expect_true("is_Th" %in% colnames(SummarizedExperiment::colData(sce2)))
  expect_true("is_NK" %in% colnames(SummarizedExperiment::colData(sce2)))
  expect_true("Th_UCell" %in% colnames(SummarizedExperiment::colData(sce2)))
  expect_true("NK_UCell" %in% colnames(SummarizedExperiment::colData(sce2)))

  # Values preserved
  expect_equal(sce2$Gated_Cells, sce$Gated_Cells)
  expect_equal(sce2$Th_UCell, sce$Th_UCell, tolerance = 1e-6)
})

test_that("Parquet round-trip preserves scGate colour entries", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  sce <- make_mock_sce()
  sce$Gated_Cells <- sample(c("Th", "NK"), ncol(sce), replace = TRUE)

  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)
  colours$Gated_Cells <- c(Th = "#FF0000", NK = "#00FF00")
  colours$is_Th <- c(Th = "steelblue")

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$smd <- data.frame(study = "test", stringsAsFactors = FALSE)
  env_save$umapDFList <- list(All = umap_df)
  env_save$coloursList <- colours

  save_parquet_data(tmp, envir = env_save)

  loaded <- load_parquet_for_shiny(file.path(tmp, "parquet"))

  expect_true("Gated_Cells" %in% names(loaded$coloursList))
  expect_equal(loaded$coloursList$Gated_Cells, colours$Gated_Cells)
  expect_true("is_Th" %in% names(loaded$coloursList))
  expect_equal(loaded$coloursList$is_Th, colours$is_Th)
})

# ── Plot rendering with scGate columns ──────────────────────────────────────

test_that("make_feature_scatter works with UCell score column", {
  sce <- make_mock_sce()
  sce$Th_UCell <- runif(ncol(sce), 0, 1)
  df <- make_mock_umap_df(sce)

  p <- make_feature_scatter(df, "Th_UCell")
  expect_s3_class(p, "ggplot")
})

test_that("make_violin_plot works with Gated_Cells grouping", {
  sce <- make_mock_sce()
  sce$Gated_Cells <- sample(c("Th", "NK"), ncol(sce), replace = TRUE)
  df <- make_mock_umap_df(sce)

  p <- make_violin_plot(df, marker = "Marker1", group_col = "Gated_Cells")
  expect_s3_class(p, "ggplot")
})
