# Tests for inst/app/helpers/plot_helpers.R

test_that("marmot_dr_theme returns a ggplot theme", {
  th <- marmot_dr_theme()
  expect_s3_class(th, "theme")
})

test_that("marmot_dr_theme with show_axes=FALSE removes axis title", {
  th <- marmot_dr_theme(show_axes = FALSE)
  expect_equal(th$axis.title, ggplot2::element_blank())
})

test_that("make_feature_scatter returns ggplot with GeomPoint", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  p <- make_feature_scatter(df, "Marker1")
  expect_s3_class(p, "ggplot")

  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPoint" %in% layer_classes)
  expect_equal(p$labels$title, "Marker1")
})

test_that("make_violin_plot returns ggplot with GeomViolin", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  p <- make_violin_plot(df, "Marker1", "cluster_id")
  expect_s3_class(p, "ggplot")

  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomViolin" %in% layer_classes)
})

test_that("make_violin_plot with split_col works", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  p <- make_violin_plot(df, "Marker1", "cluster_id", split_col = "condition")
  expect_s3_class(p, "ggplot")
})

test_that("make_dot_plot returns ggplot with GeomPoint", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)
  agg <- aggregate_expression(df, c("Marker1", "Marker2"), "cluster_id")

  p <- make_dot_plot(agg$avg_expr, agg$pct_expr)
  expect_s3_class(p, "ggplot")

  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPoint" %in% layer_classes)
})

test_that("make_dot_plot with flip=TRUE has CoordFlip", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)
  agg <- aggregate_expression(df, c("Marker1", "Marker2"), "cluster_id")

  p <- make_dot_plot(agg$avg_expr, agg$pct_expr, flip = TRUE)
  expect_s3_class(p$coordinates, "CoordFlip")
})

test_that("make_ridge_plot returns ggplot with density ridges layer", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  p <- make_ridge_plot(df, "Marker1", "cluster_id")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Marker1")

  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("Ridges|Density", layer_classes)))
})

test_that("make_percell_heatmap returns Heatmap with correct dimensions", {
  sce <- make_mock_sce()
  expr_mat <- as.matrix(SummarizedExperiment::assay(sce, "exprsQuantNorm"))
  group_ids <- sce$cluster_id
  colours <- make_mock_colours(sce)

  hm <- make_percell_heatmap(expr_mat, group_ids, group_colours = colours$cluster_id)
  expect_s4_class(hm, "Heatmap")
  expect_equal(nrow(hm@matrix), nrow(expr_mat))
})

test_that("add_facet_with_counts adds FacetWrap with counts", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()
  p2 <- add_facet_with_counts(p, df, "condition")
  expect_s3_class(p2$facet, "FacetWrap")
})

test_that("make_barplot returns ggplot", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  p <- make_barplot(df, "sample_id", "cluster_id")
  expect_s3_class(p, "ggplot")

  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomBar" %in% layer_classes)
})

test_that("make_barplot fractional mode works", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  p <- make_barplot(df, "sample_id", "cluster_id", fractional = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("make_barplot show_numbers adds GeomText", {
  sce <- make_mock_sce()
  df <- make_mock_umap_df(sce)

  p <- make_barplot(df, "sample_id", "cluster_id", show_numbers = TRUE)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomText" %in% layer_classes)
})
