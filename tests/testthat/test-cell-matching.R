# Tests for cell identity matching — positional correspondence invariants
#
# All clustering and DR paths in the MARMOT pipeline rely on positional
# correspondence: cell i in the expression matrix maps to cluster label i
# and to row i of the DR data frame. These tests verify that invariant.

# ── Local replica of pipeline's build_dr_df ──
# The pipeline defines this inline (Qmd line 1078); it isn't exported.
# Parameterised to accept sce + clusteringMethodToUse explicitly.
build_dr_df_test <- function(sce, method, clusteringMethodToUse = "meta10") {
  coords <- SingleCellExperiment::reducedDim(sce, method)[, 1:2]
  colnames(coords) <- c("x", "y")
  df <- data.frame(
    SummarizedExperiment::colData(sce),
    sce_idx = seq_len(ncol(sce)),
    coords,
    DRMethod = method,
    check.names = FALSE
  )

  # Add cluster annotations
  cc <- S4Vectors::metadata(sce)$cluster_codes
  if (!is.null(cc)) {
    df <- dplyr::left_join(df, cc, by = stats::setNames(clusteringMethodToUse, "cluster_id"))
  }
  df[[clusteringMethodToUse]] <- df$cluster_id

  # Add quantile-normalised expression
  qn <- t(as.matrix(SummarizedExperiment::assay(sce, "exprsQuantNorm")))
  df <- dplyr::bind_cols(df, as.data.frame(qn))

  # Drop only rows with missing DR coordinates

  df <- df[!is.na(df$x) & !is.na(df$y), , drop = FALSE]

  df
}

# ── Section 1: build_dr_df core invariant ──
test_that("build_dr_df: cluster_id matches SCE via sce_idx (UMAP)", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  # Every row's cluster_id should match the SCE at that index
  for (i in seq_len(nrow(df))) {
    expect_equal(
      as.character(df$cluster_id[i]),
      as.character(sce$cluster_id[df$sce_idx[i]])
    )
  }
})

test_that("build_dr_df: x/y coordinates match reducedDim via sce_idx (UMAP)", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")
  umap <- SingleCellExperiment::reducedDim(sce, "UMAP")

  expect_equal(df$x, umap[df$sce_idx, 1], ignore_attr = TRUE)
  expect_equal(df$y, umap[df$sce_idx, 2], ignore_attr = TRUE)
})

test_that("build_dr_df: x/y coordinates match reducedDim via sce_idx (TSNE)", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "TSNE")
  tsne <- SingleCellExperiment::reducedDim(sce, "TSNE")

  expect_equal(df$x, tsne[df$sce_idx, 1], ignore_attr = TRUE)
  expect_equal(df$y, tsne[df$sce_idx, 2], ignore_attr = TRUE)
})

test_that("build_dr_df: expression columns match assay via sce_idx", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")
  qn <- SummarizedExperiment::assay(sce, "exprsQuantNorm")
  markers <- rownames(qn)

  for (m in markers) {
    expect_equal(df[[m]], as.numeric(qn[m, df$sce_idx]), ignore_attr = TRUE)
  }
})

test_that("build_dr_df: sample_id matches SCE via sce_idx", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  expect_equal(
    as.character(df$sample_id),
    as.character(sce$sample_id[df$sce_idx])
  )
})

test_that("build_dr_df: condition matches SCE via sce_idx", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  expect_equal(
    as.character(df$condition),
    as.character(sce$condition[df$sce_idx])
  )
})

test_that("build_dr_df: sce_idx is 1:ncol(sce) when no NAs", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  expect_equal(df$sce_idx, seq_len(ncol(sce)))
})


# ── Section 2: NA coordinate filtering ──
test_that("NA coordinate filtering: correct nrow after removal", {
  sce <- make_cell_matching_sce(n_na_coords = 10)
  df <- build_dr_df_test(sce, "UMAP")

  expect_equal(nrow(df), ncol(sce) - 10)
})

test_that("NA coordinate filtering: remaining sce_idx values are valid", {
  sce <- make_cell_matching_sce(n_na_coords = 10)
  df <- build_dr_df_test(sce, "UMAP")

  expect_true(all(df$sce_idx >= 1 & df$sce_idx <= ncol(sce)))
  expect_equal(length(unique(df$sce_idx)), nrow(df))  # no duplicates
})

test_that("NA coordinate filtering: sce_idx subset of 1:ncol(sce)", {
  sce <- make_cell_matching_sce(n_na_coords = 10)
  df <- build_dr_df_test(sce, "UMAP")

  expect_true(all(df$sce_idx %in% seq_len(ncol(sce))))
  # First 10 cells should be absent (those were set to NA)
  expect_true(!any(1:10 %in% df$sce_idx))
})

test_that("NA coordinate filtering: expression still matches after filtering", {
  sce <- make_cell_matching_sce(n_na_coords = 10)
  df <- build_dr_df_test(sce, "UMAP")
  qn <- SummarizedExperiment::assay(sce, "exprsQuantNorm")

  marker <- rownames(qn)[1]
  expect_equal(df[[marker]], as.numeric(qn[marker, df$sce_idx]), ignore_attr = TRUE)
})


# ── Section 3: left_join with cluster_codes ──
test_that("left_join with cluster_codes: row order preserved", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  # sce_idx should be monotonically increasing (left table order preserved)
  expect_true(all(diff(df$sce_idx) > 0))
})

test_that("left_join with cluster_codes: no row duplication", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  expect_equal(nrow(df), ncol(sce))
})

test_that("left_join with cluster_codes: meta10 column present", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  expect_true("meta10" %in% colnames(df))
})


# ── Section 4: bind_cols expression alignment ──
test_that("bind_cols expression: nrow matches", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")
  qn <- SummarizedExperiment::assay(sce, "exprsQuantNorm")

  # Expression columns should have same number of rows as df
  for (m in rownames(qn)) {
    expect_equal(length(df[[m]]), nrow(df))
  }
})

test_that("bind_cols expression: spot-check specific cells", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")
  qn <- SummarizedExperiment::assay(sce, "exprsQuantNorm")
  marker <- rownames(qn)[1]

  spot_indices <- c(1, 25, 50, 75, 100)
  for (idx in spot_indices) {
    expect_equal(
      df[[marker]][idx],
      as.numeric(qn[marker, df$sce_idx[idx]])
    )
  }
})


# ── Section 5: Clustering assignment order ──
test_that("FlowSOM-style assignment: labels match back to cells", {
  sce <- make_cell_matching_sce()
  set.seed(7)
  labels <- factor(paste0("k", sample(1:10, ncol(sce), replace = TRUE)))
  sce[["meta10"]] <- labels

  # Read back — must be identical
  expect_identical(sce[["meta10"]], labels)
})

test_that("PARC-style assignment: labels match back to cells", {
  sce <- make_cell_matching_sce()
  set.seed(8)
  labels <- factor(paste0("c", sample(1:10, ncol(sce), replace = TRUE)))
  sce[["p10"]] <- labels

  expect_identical(sce[["p10"]], labels)
})

test_that("Rphenograph/MfastPG/Mphenograph-style assignment: labels match back to cells", {
  sce <- make_cell_matching_sce()
  set.seed(9)
  labels <- factor(paste0("p", sample(1:10, ncol(sce), replace = TRUE)))
  sce[["k10"]] <- labels

  expect_identical(sce[["k10"]], labels)
})

test_that("cluster_id copy preserves identity", {
  sce <- make_cell_matching_sce()
  set.seed(10)
  labels <- factor(paste0("k", sample(1:10, ncol(sce), replace = TRUE)))
  sce[["meta10"]] <- labels
  sce$cluster_id <- sce[["meta10"]]

  expect_identical(as.character(sce$cluster_id), as.character(labels))
})

test_that("factor level re-sorting doesn't change cell values", {
  sce <- make_cell_matching_sce()
  original_values <- as.character(sce$cluster_id)

  # Re-sort levels using mixedsort (as pipeline does)
  sce$cluster_id <- factor(
    sce$cluster_id,
    levels = gtools::mixedsort(levels(sce$cluster_id))
  )

  expect_equal(as.character(sce$cluster_id), original_values)
})


# ── Section 6: DR coordinate assignment ──
test_that("reducedDim assignment and read-back: exact match", {
  sce <- make_cell_matching_sce()
  set.seed(11)
  mat <- matrix(rnorm(ncol(sce) * 2), ncol = 2)
  SingleCellExperiment::reducedDim(sce, "test") <- mat

  result <- SingleCellExperiment::reducedDim(sce, "test")
  expect_equal(result, mat, ignore_attr = TRUE)
})

test_that("reducedDim: identity-pattern coordinates round-trip correctly", {
  sce <- make_cell_matching_sce()
  n <- ncol(sce)
  mat <- cbind(seq_len(n), -seq_len(n))
  SingleCellExperiment::reducedDim(sce, "identity") <- mat

  result <- SingleCellExperiment::reducedDim(sce, "identity")
  # Cell 1 should be (1, -1), cell 50 should be (50, -50)
  expect_equal(result[1, ], c(1, -1))
  expect_equal(result[50, ], c(50, -50))
  expect_equal(result[n, ], c(n, -n))
})

test_that("multiple reducedDims are independent and correctly ordered", {
  sce <- make_cell_matching_sce()
  umap_orig <- SingleCellExperiment::reducedDim(sce, "UMAP")
  tsne_orig <- SingleCellExperiment::reducedDim(sce, "TSNE")

  # They should be different
  expect_false(identical(umap_orig, tsne_orig))

  # Each should be retrievable independently
  expect_equal(
    SingleCellExperiment::reducedDim(sce, "UMAP"),
    umap_orig
  )
  expect_equal(
    SingleCellExperiment::reducedDim(sce, "TSNE"),
    tsne_orig
  )
})


# ── Section 7: Downsampling preserves sce_idx ──
test_that("downsampled df: sce_idx values still valid", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  set.seed(12)
  sub <- df[sample(nrow(df), 50), ]

  expect_true(all(sub$sce_idx %in% seq_len(ncol(sce))))
})

test_that("downsampled df: cluster_id matches SCE at subsetted indices", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  set.seed(13)
  sub <- df[sample(nrow(df), 50), ]

  expect_equal(
    as.character(sub$cluster_id),
    as.character(sce$cluster_id[sub$sce_idx])
  )
})

test_that("downsampled df: coordinates and expression match at subsetted indices", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")
  umap <- SingleCellExperiment::reducedDim(sce, "UMAP")
  qn <- SummarizedExperiment::assay(sce, "exprsQuantNorm")

  set.seed(14)
  sub <- df[sample(nrow(df), 50), ]

  expect_equal(sub$x, umap[sub$sce_idx, 1], ignore_attr = TRUE)
  expect_equal(sub$y, umap[sub$sce_idx, 2], ignore_attr = TRUE)

  marker <- rownames(qn)[1]
  expect_equal(sub[[marker]], as.numeric(qn[marker, sub$sce_idx]), ignore_attr = TRUE)
})


# ── Section 8: extract_dr_df Shiny helper ──
test_that("extract_dr_df: x/y match reducedDim row-for-row", {
  sce <- make_cell_matching_sce()
  df <- extract_dr_df(sce, "UMAP")
  umap <- SingleCellExperiment::reducedDim(sce, "UMAP")

  expect_equal(df$x, umap[, 1], ignore_attr = TRUE)
  expect_equal(df$y, umap[, 2], ignore_attr = TRUE)
})

test_that("extract_dr_df: cluster_id matches colData row-for-row", {
  sce <- make_cell_matching_sce()
  df <- extract_dr_df(sce, "UMAP")

  expect_equal(
    as.character(df$cluster_id),
    as.character(SummarizedExperiment::colData(sce)$cluster_id)
  )
})

test_that("extract_dr_df: nrow equals ncol(sce)", {
  sce <- make_cell_matching_sce()
  df <- extract_dr_df(sce, "UMAP")

  expect_equal(nrow(df), ncol(sce))
})


# ── Section 9: h5ad round-trip ──
test_that("h5ad round-trip: colnames preserved", {
  skip_if_not_installed("anndataR")
  sce_orig <- make_cell_matching_sce()
  tmp <- tempfile("h5ad_test_")
  dir.create(tmp)

  env <- new.env(parent = emptyenv())
  env$sce <- sce_orig

  save_h5ad_data(tmp, envir = env)
  sce_loaded <- reconstruct_sce_from_h5ad(file.path(tmp, "marmot_results.h5ad"))

  expect_equal(colnames(sce_loaded), colnames(sce_orig))

  unlink(tmp, recursive = TRUE)
})

test_that("h5ad round-trip: cluster_id and sample_id match cell-by-cell", {
  skip_if_not_installed("anndataR")
  sce_orig <- make_cell_matching_sce()
  tmp <- tempfile("h5ad_test_")
  dir.create(tmp)

  env <- new.env(parent = emptyenv())
  env$sce <- sce_orig

  save_h5ad_data(tmp, envir = env)
  sce_loaded <- reconstruct_sce_from_h5ad(file.path(tmp, "marmot_results.h5ad"))

  expect_equal(
    as.character(sce_loaded$cluster_id),
    as.character(sce_orig$cluster_id)
  )
  expect_equal(
    as.character(sce_loaded$sample_id),
    as.character(sce_orig$sample_id)
  )

  unlink(tmp, recursive = TRUE)
})

test_that("h5ad round-trip: reducedDim coordinates match cell-by-cell", {
  skip_if_not_installed("anndataR")
  sce_orig <- make_cell_matching_sce()
  tmp <- tempfile("h5ad_test_")
  dir.create(tmp)

  env <- new.env(parent = emptyenv())
  env$sce <- sce_orig

  save_h5ad_data(tmp, envir = env)
  sce_loaded <- reconstruct_sce_from_h5ad(file.path(tmp, "marmot_results.h5ad"))

  umap_orig <- SingleCellExperiment::reducedDim(sce_orig, "UMAP")
  umap_loaded <- SingleCellExperiment::reducedDim(sce_loaded, "UMAP")
  expect_equal(umap_loaded, umap_orig, ignore_attr = TRUE)

  tsne_orig <- SingleCellExperiment::reducedDim(sce_orig, "TSNE")
  tsne_loaded <- SingleCellExperiment::reducedDim(sce_loaded, "TSNE")
  expect_equal(tsne_loaded, tsne_orig, ignore_attr = TRUE)

  unlink(tmp, recursive = TRUE)
})

test_that("h5ad round-trip: DR df sce_idx valid against reloaded SCE", {
  skip_if_not_installed("anndataR")
  sce_orig <- make_cell_matching_sce()
  tmp <- tempfile("h5ad_test_")
  dir.create(tmp)

  df_orig <- build_dr_df_test(sce_orig, "UMAP")
  env <- new.env(parent = emptyenv())
  env$sce <- sce_orig
  env$umapDFList <- list(UMAP = df_orig)

  save_h5ad_data(tmp, envir = env)
  sce_loaded <- reconstruct_sce_from_h5ad(file.path(tmp, "marmot_results.h5ad"))

  expect_true(all(df_orig$sce_idx %in% seq_len(ncol(sce_loaded))))
  expect_equal(
    as.character(df_orig$cluster_id),
    as.character(sce_loaded$cluster_id[df_orig$sce_idx])
  )

  unlink(tmp, recursive = TRUE)
})

test_that("h5ad round-trip: cluster_codes preserved in SCE metadata", {
  skip_if_not_installed("anndataR")
  sce_orig <- make_cell_matching_sce()
  tmp <- tempfile("h5ad_test_")
  dir.create(tmp)

  env <- new.env(parent = emptyenv())
  env$sce <- sce_orig

  save_h5ad_data(tmp, envir = env)
  sce_loaded <- reconstruct_sce_from_h5ad(file.path(tmp, "marmot_results.h5ad"))

  cc_orig <- S4Vectors::metadata(sce_orig)$cluster_codes
  cc_loaded <- S4Vectors::metadata(sce_loaded)$cluster_codes

  expect_false(is.null(cc_loaded))
  expect_equal(colnames(cc_loaded), colnames(cc_orig))
  expect_equal(
    as.character(cc_loaded[[1]]),
    as.character(cc_orig[[1]])
  )

  unlink(tmp, recursive = TRUE)
})


# ── Section 10: Relabelling and DA filter ──
test_that("apply_relabelling_pure: row order and sce_idx unchanged", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")
  colours <- make_mock_colours(sce)
  umapDFList <- list(UMAP = df)

  cluster_table <- data.frame(
    relabelled_clusters = paste0("R_", levels(sce$cluster_id)),
    colours = scales::hue_pal()(nlevels(sce$cluster_id)),
    row.names = levels(sce$cluster_id),
    stringsAsFactors = FALSE
  )

  result <- apply_relabelling_pure(sce, umapDFList, colours, cluster_table)

  # sce_idx must be unchanged
  expect_equal(result$umapDFList$UMAP$sce_idx, df$sce_idx)
  # Row count unchanged
  expect_equal(nrow(result$umapDFList$UMAP), nrow(df))
  # cluster_id values unchanged (relabelling adds a new column)
  expect_equal(
    as.character(result$umapDFList$UMAP$cluster_id),
    as.character(df$cluster_id)
  )
})

test_that("filter_da_clusters in 'None' mode: df intact", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  result <- filter_da_clusters(df, list(c("c1"), c("c2")), mode = "None")

  expect_equal(nrow(result$umap_df), nrow(df))
  expect_equal(result$umap_df$sce_idx, df$sce_idx)
  expect_null(result$warning)
})

test_that("filter_da_clusters in 'All' mode: preserves row count and sce_idx", {
  sce <- make_cell_matching_sce()
  df <- build_dr_df_test(sce, "UMAP")

  da_clusters <- list(c("c1", "c2"), c("c3"))
  result <- filter_da_clusters(df, da_clusters, mode = "All")

  # Row count unchanged (DA filter only relabels, doesn't drop rows)
  expect_equal(nrow(result$umap_df), nrow(df))
  expect_equal(result$umap_df$sce_idx, df$sce_idx)
})
