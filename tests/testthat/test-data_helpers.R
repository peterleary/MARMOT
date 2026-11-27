# Tests for inst/app/helpers/data_helpers.R

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

# ── extract_expr_matrix ─────────────────────────────────────────────────────

test_that("extract_expr_matrix: NULL markers returns full assay", {
  sce <- make_mock_sce()
  result <- extract_expr_matrix(sce, "exprsTransformed", markers = NULL)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(sce))
  expect_equal(ncol(result), ncol(sce))
  expect_equal(rownames(result), rownames(sce))
})

test_that("extract_expr_matrix: subsetting by marker preserves order", {
  sce <- make_mock_sce()
  result <- extract_expr_matrix(sce, "exprsTransformed",
                                markers = c("Marker3", "Marker1"))

  expect_equal(rownames(result), c("Marker3", "Marker1"))
  expect_equal(ncol(result), ncol(sce))
})

test_that("extract_expr_matrix: missing assay falls back to first", {
  sce <- make_mock_sce()
  result <- extract_expr_matrix(sce, assay_name = "no_such_assay",
                                markers = "Marker1")

  expect_equal(nrow(result), 1L)
  expect_equal(rownames(result), "Marker1")
})

test_that("extract_expr_matrix: unmatched markers dropped silently", {
  sce <- make_mock_sce()
  result <- extract_expr_matrix(sce, "exprsTransformed",
                                markers = c("Marker1", "NOTAMARKER"))

  expect_equal(rownames(result), "Marker1")
  expect_equal(nrow(result), 1L)
})

test_that("extract_expr_matrix: all markers unmatched returns empty (not full) matrix", {
  sce <- make_mock_sce()
  result <- extract_expr_matrix(sce, "exprsTransformed",
                                markers = c("NOTAMARKER", "ALSO_NO"))

  # Critical: must NOT silently return the full matrix
  expect_equal(nrow(result), 0L)
  expect_equal(ncol(result), ncol(sce))
})

test_that("extract_expr_matrix: hyphen/underscore substitution finds markers", {
  sce <- make_mock_sce()
  # Add a marker with hyphenated name to the row dimnames
  rownames(sce)[1] <- "Marker_1"
  result <- extract_expr_matrix(sce, "exprsTransformed",
                                markers = "Marker-1")

  expect_equal(rownames(result), "Marker_1")
  expect_equal(nrow(result), 1L)
})

test_that("extract_expr_matrix: values match the underlying assay", {
  sce <- make_mock_sce()
  result <- extract_expr_matrix(sce, "exprsTransformed", markers = "Marker2")
  expected <- as.matrix(SummarizedExperiment::assay(sce, "exprsTransformed"))["Marker2", , drop = FALSE]

  expect_equal(unname(result), unname(expected))
})

# ── tidy_contrasts / get_da_clusters ─────────────────────────────────────────

test_that("tidy_contrasts matches the pipeline: blanks, spacing and non-contrasts go", {
  raw <- c("B over A", NA, "  C   over  A ", "", "not a contrast", "D over B")
  expect_equal(tidy_contrasts(raw), c("B over A", "C over A", "D over B"))
  expect_equal(tidy_contrasts(NULL), character(0))
})

test_that("results_contrasts: a contrast typed 'A vs B' is found as the pipeline read it", {
  # The sheet keeps what was typed; the DA tables carry what the pipeline read
  res <- list(smd = data.frame(`Conditions To Test` = c("treated vs control", "KO over WT"), check.names = FALSE),
              daList = list("Treated over Control" = data.frame(), "KO over WT" = data.frame()))
  expect_equal(results_contrasts(res), c("Treated over Control", "KO over WT"))
  scl <- list("Contrast 1: Treated Up" = "3", "Contrast 1: Control Up" = "5", "Contrast 2: KO Up" = "7")
  expect_equal(get_da_clusters(scl, results_contrasts(res), "Treated over Control"), list(up = "3", down = "5"))
  expect_equal(get_da_clusters(scl, results_contrasts(res), "KO over WT")$up, "7")
  # Without DA tables the typed column is still used
  expect_equal(results_contrasts(list(smd = res$smd)), "KO over WT")
})

test_that("get_da_clusters: a list in pipeline order is read correctly", {
  contrasts <- c("B over A", "C over A")
  scl <- list(
    "Contrast 1: B Up" = c("1", "2"),
    "Contrast 1: A Up" = c("3"),
    "Contrast 2: C Up" = c("4"),
    "Contrast 2: A Up" = c("5", "6")
  )
  expect_equal(get_da_clusters(scl, contrasts, "B over A"),
               list(up = c("1", "2"), down = "3"))
  expect_equal(get_da_clusters(scl, contrasts, "C over A"),
               list(up = "4", down = c("5", "6")))
})

test_that("get_da_clusters: empty entries left out of the file do not shift the rest", {
  # As loaded from h5ad: 'Contrast 1: B Up' was empty and is absent, and the
  # remaining entries come back in alphabetical order.
  contrasts <- c("B over A", "C over A")
  scl <- list(
    "Contrast 1: A Up" = c("3"),
    "Contrast 2: A Up" = c("5", "6"),
    "Contrast 2: C Up" = c("4")
  )
  expect_equal(get_da_clusters(scl, contrasts, "B over A"),
               list(up = character(0), down = "3"))
  expect_equal(get_da_clusters(scl, contrasts, "C over A"),
               list(up = "4", down = c("5", "6")))
})

test_that("get_da_clusters: alphabetical order cannot swap up and down", {
  # 'A' sorts before 'B', so position would call the 'A Up' clusters 'up'.
  scl <- list("Contrast 1: A Up" = "9", "Contrast 1: B Up" = "7")
  expect_equal(get_da_clusters(scl, "B over A", "B over A"),
               list(up = "7", down = "9"))
})

test_that("get_da_clusters: nothing significant, unknown contrast or no list give empties", {
  none <- list(up = character(0), down = character(0))
  expect_equal(get_da_clusters(list(), "B over A", "B over A"), none)
  expect_equal(get_da_clusters(NULL, "B over A", "B over A"), none)
  expect_equal(get_da_clusters(list("Contrast 1: B Up" = "1"), "B over A", "None"), none)
})

test_that("get_da_clusters: numbering ignores rows the pipeline ignores", {
  # The pipeline skips the middle row, so 'C over A' is its Contrast 2.
  contrasts <- c("B over A", "just a note", "C  over A")
  scl <- list("Contrast 2: C Up" = "4")
  expect_equal(get_da_clusters(scl, contrasts, "C over A")$up, "4")
})

test_that("get_da_clusters: a condition whose name contains ' over ' is split correctly", {
  scl <- list("Contrast 1: left over Up" = "1", "Contrast 1: ctrl Up" = c("2", "3"))
  expect_equal(get_da_clusters(scl, "left over over ctrl", "left over over ctrl"),
               list(up = "1", down = c("2", "3")))
})

test_that("filter_da_clusters: a cluster listed as both up and down does not break 'All'", {
  df <- data.frame(cluster_id = c("c1", "c2", "c3"), x = 1:3, y = 1:3, stringsAsFactors = FALSE)
  result <- filter_da_clusters(df, list(c("c1", "c2"), c("c2")), mode = "All")
  expect_equal(levels(result$umap_df$cluster_id), c("c1", "c2", "Other"))
})

test_that("aggregate_expression: the positive share uses the given values and threshold", {
  # Displayed values are all above zero (as quantile-normalised values are);
  # positivity is judged on separate values against a threshold of 1.
  shown    <- data.frame(M = c(0.1, 0.2, 0.9, 0.8, 0.5, 0.6), group = rep(c("a", "b"), each = 3))
  positive <- data.frame(M = c(0.2, 0.5, 2.0, 3.0, 1.5, 0.9))
  res <- aggregate_expression(shown, "M", "group", positive_df = positive, threshold = 1)
  expect_equal(unname(res$pct_expr[, "M"]), c(100 / 3, 200 / 3))
  expect_equal(unname(res$avg_expr[, "M"]), c(mean(c(0.1, 0.2, 0.9)), mean(c(0.8, 0.5, 0.6))))
  # Scaling the displayed values must not move the percentage
  shown$M <- as.numeric(scale(shown$M))
  expect_equal(aggregate_expression(shown, "M", "group", positive_df = positive, threshold = 1)$pct_expr,
               res$pct_expr)
})

test_that("the Explorer opens on saved labels and saves unchanged names blank", {
  tbl <- data.frame(relabelled_clusters = c("1", "2", "3"), colours = "#000000",
                    row.names = c("1", "2", "3"), stringsAsFactors = FALSE)
  sheet <- data.frame(Cluster = c("1", "2", "3"), Cells = c(10, 20, 30),
                      `Most distinctive markers` = c("CD3", "CD19", ""),
                      Label = c("T cells", "", "drop"), check.names = FALSE)
  added <- add_labels_sheet(tbl, sheet)
  expect_true(added$labelled)
  expect_identical(added$table$relabelled_clusters, c("T cells", "2", "drop"))
  expect_identical(added$table$cells, c(10, 20, 30))
  expect_identical(added$table$markers, c("CD3", "CD19", ""))
  expect_identical(labels_to_save(added$table), c("1" = "T cells", "2" = "", "3" = "drop"))
  sheet$Label <- ""
  expect_false(add_labels_sheet(tbl, sheet)$labelled)
})

test_that("the Explorer lists every map once, with the report's version first", {
  df <- function(n) data.frame(x = seq_len(n))
  L <- list(UMAP = df(10), UMAP_uncorrected = df(10), Downsampled.UMAP = df(4),
            Downsampled.UMAP_uncorrected = df(4), All = df(10), Downsampled = df(4))
  ch <- dr_map_choices(L)
  expect_identical(unname(ch), c("Downsampled.UMAP", "UMAP", "Downsampled.UMAP_uncorrected", "UMAP_uncorrected"))
  expect_identical(names(ch), c("UMAP|as in the report", "UMAP|every cell",
                                "UMAP|before batch correction, as in the report",
                                "UMAP|before batch correction, every cell"))
  expect_identical(dr_map_default(ch), "Downsampled.UMAP")
  expect_identical(nrow(dr_map_frame(L, "nonsense")), 4L)
  L$Downsampled.UMAP <- df(10)   # a subsample holding every cell: listed once
  expect_identical(unname(dr_map_choices(L))[1], "UMAP")
})

test_that("the Explorer merges names as the MARMOT extension does", {
  expect_identical(same_name_spelling(c("T cells", "B cells", "T-cells", "t_cells", "drop")),
                   c("T cells", "B cells", "T cells", "T cells", "drop"))
})

test_that("subsetting never crashes on empty or negative targets, and repeats", {
  md <- data.frame(g = rep(c("a", "b"), each = 5), row.names = paste0("c", 1:10))
  expect_length(sample_cells_by_group(md, "g", c(a = NA, b = 2)), 7)    # empty box: the whole group
  expect_length(sample_cells_by_group(md, "g", c(a = -5, b = 2)), 2)    # below zero: none
  expect_length(sample_cells_by_group(md, "g", c(a = 99, b = 1)), 6)    # more than there are: all
  expect_identical(sample_cells_by_group(md, "g", c(a = 3, b = 3)),
                   sample_cells_by_group(md, "g", c(a = 3, b = 3)))     # same settings, same cells
  set.seed(1); r1 <- runif(1); set.seed(1); invisible(sample_cells_by_group(md, "g", c(a = 3))); r2 <- runif(1)
  expect_identical(r1, r2)                                              # the session's random state is untouched
})

test_that("groups in neither list are kept; excluded groups are not", {
  v <- factor(c("T cells", "B cells", "NK", "drop"))
  expect_identical(bucket_levels(v, include = c("NK", "1"), exclude = "drop"), c("NK", "B cells", "T cells"))
  expect_identical(bucket_levels(v, include = NULL, exclude = NULL), levels(v))
})

test_that("clusterings stay plottable however many clusters they have", {
  cd <- data.frame(cluster_id = factor(1:150), k60 = factor(1:150), cell = 1:150, condition = rep(c("A", "B"), 75))
  expect_identical(get_plottable_columns(cd), c("cluster_id", "k60", "condition"))
})

test_that("the map table keeps only what the map needs, in the same order every time", {
  df <- data.frame(x = 1:20, y = 20:1, cluster_id = rep(c("a", "b"), 10), CD3 = runif(20), CD4 = runif(20))
  a <- map_draw_table(df, c("cluster_id", "condition"))
  expect_identical(names(a), c("x", "y", "cluster_id"))
  expect_identical(a, map_draw_table(df, c("cluster_id", "condition")))
  expect_setequal(a$x, 1:20)
})

test_that("labelled populations become a column with their first cluster's colour", {
  rec <- data.frame(cluster = c("1", "2", "3", "4"), population = c("T cells", "T cells", "3", "dropped"),
                    dropped = c(FALSE, FALSE, FALSE, TRUE), cells = rep(5L, 4), stringsAsFactors = FALSE)
  cols <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "black", "5" = "pink")
  lp <- labelled_population_column(c("1", "2", "3", "4", "5"), rec, cols)
  expect_identical(as.character(lp$population), c("T cells", "T cells", "3", "dropped", "5"))
  expect_identical(levels(lp$population), c("T cells", "3", "5", "dropped"))
  expect_identical(unname(lp$colours), c("red", "green", "pink", "grey85"))
  expect_null(labelled_population_column("1", rec)$colours)
  # Offered wherever clusterings are, however many populations
  expect_true("population" %in% get_plottable_columns(data.frame(population = as.character(1:150))))
})

test_that("markers' display names come from the study sheet, blanks left out", {
  smd <- data.frame(`Markers to include` = c("IFNg", "CD4", NA), `Display names for markers` = c("IFN-g", "", NA),
                    check.names = FALSE)
  expect_identical(marker_display_names(smd), c(IFNg = "IFN-g"))
  expect_identical(marker_display_names(smd[, 1, drop = FALSE]), character())
  expect_identical(marker_display_names(NULL), character())
})

test_that("markers_in_cells finds markers listed several to a cell", {
  valid <- c("CD3", "CD4", "CD8a", "HLA DR", "Ki67")
  expect_equal(markers_in_cells("CD3 CD4 CD8a", valid), c("CD3", "CD4", "CD8a"))
  expect_equal(markers_in_cells(c("Ki67", "CD4, CD3"), valid), c("Ki67", "CD4", "CD3"))
  expect_equal(markers_in_cells("CD3 HLA DR", valid), c("CD3", "HLA DR"))
  expect_equal(markers_in_cells(c("Cluster 1", NA), valid), character(0))
})

