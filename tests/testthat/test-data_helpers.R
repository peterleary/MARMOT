# Tests for inst/app/helpers/data_helpers.R

test_that("detect_data_format identifies h5ad", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "marmot_results.h5ad"))

  expect_equal(detect_data_format(tmp), "h5ad")
})

test_that("detect_data_format errors when only qs files present", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "sce.qs"))

  expect_error(detect_data_format(tmp), "No MARMOT data found")
})

test_that("detect_data_format errors on empty directory", {
  tmp <- withr::local_tempdir()

  expect_error(detect_data_format(tmp), "No MARMOT data found")
})

test_that("extract_marker_data returns correct columns", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  result <- extract_marker_data(df, c("Marker1", "Marker2"))
  expect_true(all(c("x", "y", "Marker1", "Marker2") %in% colnames(result)))
  expect_equal(nrow(result), nrow(df))
})

test_that("extract_marker_data returns NULL for missing markers", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  result <- extract_marker_data(df, c("NOTAMARKER", "ALSO_NO"))
  expect_null(result)
})

test_that("extract_marker_data handles partial overlap", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  result <- extract_marker_data(df, c("Marker1", "NOTAMARKER"))
  expect_true("Marker1" %in% colnames(result))
  expect_false("NOTAMARKER" %in% colnames(result))
})

test_that("extract_marker_data includes metadata_cols", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  result <- extract_marker_data(df, "Marker1", metadata_cols = c("cluster_id", "sample_id"))
  expect_true(all(c("cluster_id", "sample_id") %in% colnames(result)))
})

test_that("compute_label_positions gives correct medians", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  result <- compute_label_positions(df, "cluster_id")
  expect_true(all(c("cluster_id", "x", "y") %in% colnames(result)))
  expect_equal(nrow(result), length(unique(df$cluster_id)))
})

test_that("compute_label_positions with value_col adds stats", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  result <- compute_label_positions(df, "cluster_id", value_col = "Marker1")
  expect_true(all(c("median", "mean", "max") %in% colnames(result)))
})

test_that("compute_label_positions handles single group", {
  df <- data.frame(x = 1:5, y = 6:10, group = "A")
  result <- compute_label_positions(df, "group")
  expect_equal(nrow(result), 1)
  expect_equal(result$x, median(1:5))
})

test_that("aggregate_expression computes correct means and percents", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  result <- aggregate_expression(df, c("Marker1", "Marker2"), "cluster_id")
  expect_true(is.matrix(result$avg_expr))
  expect_true(is.matrix(result$pct_expr))
  expect_equal(ncol(result$avg_expr), 2)
  expect_equal(nrow(result$avg_expr), length(unique(df$cluster_id)))
})

test_that("aggregate_expression: all-zero marker gives 0 pct", {
  df <- data.frame(group = c("A", "A", "B", "B"), zero_marker = c(0, 0, 0, 0))
  result <- aggregate_expression(df, "zero_marker", "group")
  expect_true(all(result$pct_expr[, "zero_marker"] == 0))
})

test_that("calculate_proportional_subset proportions sum correctly", {
  df <- data.frame(group = rep(c("A", "B"), each = 50))
  result <- calculate_proportional_subset(df, "group", 20)
  expect_equal(sum(result), 20)
})

test_that("calculate_proportional_subset: tiny total gives at least 1 per group", {
  df <- data.frame(group = rep(c("A", "B", "C"), each = 100))
  result <- calculate_proportional_subset(df, "group", 2)
  expect_true(all(result >= 1))
})

test_that("get_plottable_columns excludes high-cardinality columns", {
  df <- data.frame(
    low_card = factor(rep(c("a", "b"), 50)),
    high_card = paste0("id_", 1:100)
  )
  result <- get_plottable_columns(df)
  expect_true("low_card" %in% result)
  expect_false("high_card" %in% result)
})

test_that("get_plottable_columns works with SCE", {
  sce <- make_mock_sce()
  result <- get_plottable_columns(sce)
  expect_true("cluster_id" %in% result)
  expect_true("sample_id" %in% result)
})

# ── apply_relabelling_pure ───────────────────────────────────────────────────

test_that("apply_relabelling_pure: basic rename", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)
  umapDFList <- list(All = umap_df, Downsampled = umap_df)

  cluster_table <- data.frame(
    relabelled_clusters = c("T cells", "B cells", "NK cells", "Mono", "DC"),
    colours = scales::hue_pal()(5),
    stringsAsFactors = FALSE
  )
  rownames(cluster_table) <- levels(sce$cluster_id)

  result <- apply_relabelling_pure(sce, umapDFList, colours, cluster_table)

  expect_true("relabelled_clusters" %in% colnames(SummarizedExperiment::colData(result$sce)))
  expect_s3_class(result$sce$relabelled_clusters, "factor")
  expect_true("relabelled_clusters" %in% names(result$coloursList))
  expect_true("relabelled_clusters" %in% colnames(result$umapDFList$All))
  expect_true("relabelled_clusters" %in% colnames(result$umapDFList$Downsampled))
})

test_that("apply_relabelling_pure: duplicate relabel names deduplicate colours", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)
  umapDFList <- list(All = umap_df)

  # Merge c1 and c2 into same label "Lymphocytes"
  cluster_table <- data.frame(
    relabelled_clusters = c("Lymphocytes", "Lymphocytes", "NK", "Mono", "DC"),
    colours = c("red", "blue", "green", "orange", "purple"),
    stringsAsFactors = FALSE
  )
  rownames(cluster_table) <- levels(sce$cluster_id)

  result <- apply_relabelling_pure(sce, umapDFList, colours, cluster_table)

  # Deduplicated: only first occurrence of "Lymphocytes" kept
  expect_equal(length(unique(names(result$coloursList$relabelled_clusters))),
               length(result$coloursList$relabelled_clusters))
  # Factor levels should contain "Lymphocytes" only once
  expect_equal(sum(levels(result$sce$relabelled_clusters) == "Lymphocytes"), 1)
})

test_that("apply_relabelling_pure: mapping is correct per cell", {
  sce <- make_mock_sce()
  umap_df <- make_mock_umap_df(sce)
  colours <- make_mock_colours(sce)
  umapDFList <- list(All = umap_df)

  cluster_table <- data.frame(
    relabelled_clusters = c("T", "B", "NK", "Mono", "DC"),
    colours = scales::hue_pal()(5),
    stringsAsFactors = FALSE
  )
  rownames(cluster_table) <- levels(sce$cluster_id)

  result <- apply_relabelling_pure(sce, umapDFList, colours, cluster_table)

  # Check a few cells: their relabelled_clusters should match the mapping
  for (cid in rownames(cluster_table)) {
    expected_label <- cluster_table[cid, "relabelled_clusters"]
    cell_idx <- which(sce$cluster_id == cid)
    actual <- as.character(result$sce$relabelled_clusters[cell_idx])
    expect_true(all(actual == expected_label))
  }
})

# ── filter_da_clusters ───────────────────────────────────────────────────────

test_that("filter_da_clusters: mode None just factors cluster_id", {
  df <- data.frame(
    cluster_id = c("c3", "c1", "c2", "c1", "c3"),
    x = rnorm(5), y = rnorm(5),
    stringsAsFactors = FALSE
  )
  result <- filter_da_clusters(df, list(c("c1"), c("c2")), mode = "None")

  expect_s3_class(result$umap_df$cluster_id, "factor")
  expect_null(result$warning)
  expect_true(all(c("c1", "c2", "c3") %in% levels(result$umap_df$cluster_id)))
  expect_false("Other" %in% levels(result$umap_df$cluster_id))
})

test_that("filter_da_clusters: mode All replaces non-DA with Other", {
  df <- data.frame(
    cluster_id = c("c1", "c2", "c3", "c4"),
    x = 1:4, y = 1:4,
    stringsAsFactors = FALSE
  )
  clusters <- list(c("c1"), c("c2"))  # up = c1, down = c2
  result <- filter_da_clusters(df, clusters, mode = "All")

  expect_null(result$warning)
  expect_equal(as.character(result$umap_df$cluster_id), c("c1", "c2", "Other", "Other"))
  # DA clusters should come first in levels, then "Other"
  lvls <- levels(result$umap_df$cluster_id)
  expect_equal(lvls[length(lvls)], "Other")
  expect_true("c1" %in% lvls)
  expect_true("c2" %in% lvls)
})

test_that("filter_da_clusters: mode Up only keeps only up clusters", {
  df <- data.frame(
    cluster_id = c("c1", "c2", "c3"),
    x = 1:3, y = 1:3,
    stringsAsFactors = FALSE
  )
  clusters <- list(c("c1"), c("c2"))
  result <- filter_da_clusters(df, clusters, mode = "Up only")

  expect_null(result$warning)
  expect_equal(as.character(result$umap_df$cluster_id), c("c1", "Other", "Other"))
})

test_that("filter_da_clusters: mode Down only keeps only down clusters", {
  df <- data.frame(
    cluster_id = c("c1", "c2", "c3"),
    x = 1:3, y = 1:3,
    stringsAsFactors = FALSE
  )
  clusters <- list(c("c1"), c("c2"))
  result <- filter_da_clusters(df, clusters, mode = "Down only")

  expect_null(result$warning)
  expect_equal(as.character(result$umap_df$cluster_id), c("Other", "c2", "Other"))
})

test_that("filter_da_clusters: empty cluster list returns warning", {
  df <- data.frame(
    cluster_id = c("c1", "c2"),
    x = 1:2, y = 1:2,
    stringsAsFactors = FALSE
  )
  clusters <- list(character(0), character(0))
  result <- filter_da_clusters(df, clusters, mode = "All")

  expect_false(is.null(result$warning))
  expect_match(result$warning, "no DA clusters")
  expect_false("Other" %in% levels(result$umap_df$cluster_id))
})

test_that("filter_da_clusters: empty up list returns warning for Up only", {
  df <- data.frame(
    cluster_id = c("c1", "c2"),
    x = 1:2, y = 1:2,
    stringsAsFactors = FALSE
  )
  clusters <- list(character(0), c("c2"))
  result <- filter_da_clusters(df, clusters, mode = "Up only")

  expect_false(is.null(result$warning))
  expect_match(result$warning, "no up DA clusters")
})

# ── build_cluster_codes ──────────────────────────────────────────────────────

test_that("build_cluster_codes: basic without relabelled", {
  df <- data.frame(
    cluster_id = factor(c("c1", "c2", "c3", "c1"), levels = c("c1", "c2", "c3")),
    x = 1:4
  )
  result <- build_cluster_codes(df)

  expect_equal(nrow(result), 3)
  expect_equal(result$cluster_ids, c("c1", "c2", "c3"))
  expect_equal(result$cluster_id_codes, 1:3)
  expect_false("relabelled_clusters" %in% colnames(result))
})

test_that("build_cluster_codes: with relabelled_clusters", {
  df <- data.frame(
    cluster_id = factor(c("c1", "c2", "c3", "c1"), levels = c("c1", "c2", "c3")),
    relabelled_clusters = factor(c("T", "B", "NK", "T")),
    x = 1:4
  )
  result <- build_cluster_codes(df)

  expect_equal(nrow(result), 3)
  expect_true("relabelled_clusters" %in% colnames(result))
  expect_true("new_cluster_codes" %in% colnames(result))
  expect_equal(as.character(result$relabelled_clusters), c("T", "B", "NK"))
  expect_true(is.numeric(result$new_cluster_codes))
})

test_that("build_cluster_codes: single cluster", {
  df <- data.frame(
    cluster_id = factor(c("c1", "c1"), levels = "c1"),
    x = 1:2
  )
  result <- build_cluster_codes(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$cluster_id_codes, 1)
})

# ── sample_cells_by_group ────────────────────────────────────────────────────

test_that("sample_cells_by_group: returns correct total", {
  set.seed(42)
  md <- data.frame(
    group = rep(c("A", "B"), each = 50),
    row.names = paste0("cell", 1:100)
  )
  cells_per_group <- c(A = 10, B = 10)
  result <- sample_cells_by_group(md, "group", cells_per_group)

  expect_equal(length(result), 20)
  expect_true(is.character(result))
  expect_true(all(result %in% rownames(md)))
})

test_that("sample_cells_by_group: caps at available cells", {
  set.seed(42)
  md <- data.frame(
    group = c("A", "A", "B", "B", "B"),
    row.names = paste0("cell", 1:5)
  )
  cells_per_group <- c(A = 100, B = 100)  # request more than available
  result <- sample_cells_by_group(md, "group", cells_per_group)

  expect_equal(length(result), 5)  # gets all cells
})

test_that("sample_cells_by_group: each group gets correct count", {
  set.seed(42)
  md <- data.frame(
    group = rep(c("X", "Y", "Z"), each = 30),
    row.names = paste0("cell", 1:90)
  )
  cells_per_group <- c(X = 5, Y = 10, Z = 15)
  result <- sample_cells_by_group(md, "group", cells_per_group)

  expect_equal(length(result), 30)
  # Check per-group counts by matching back
  group_of_result <- md[result, "group"]
  expect_equal(sum(group_of_result == "X"), 5)
  expect_equal(sum(group_of_result == "Y"), 10)
  expect_equal(sum(group_of_result == "Z"), 15)
})

test_that("sample_cells_by_group: returns character vector of cell IDs", {
  md <- data.frame(
    group = c("A", "A"),
    row.names = c("cell_1", "cell_2")
  )
  result <- sample_cells_by_group(md, "group", c(A = 1))

  expect_true(is.character(result))
  expect_equal(length(result), 1)
  expect_true(result %in% c("cell_1", "cell_2"))
})
