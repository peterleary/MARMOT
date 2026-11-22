# Tests for R/dr_frames.R

# The plotting tables as the pipeline builds them (chunk `analysis_plotting_tables`).
pipeline_dr_frames <- function(sce, clustering_method) {
  build_dr_df <- function(method) {
    coords <- SingleCellExperiment::reducedDim(sce, method)[, 1:2]
    colnames(coords) <- c("x", "y")
    df <- data.frame(SummarizedExperiment::colData(sce), sce_idx = seq_len(ncol(sce)),
                     coords, DRMethod = method, check.names = FALSE)
    if (!is.null(S4Vectors::metadata(sce)$cluster_codes)) {
      df <- dplyr::left_join(df, S4Vectors::metadata(sce)$cluster_codes, by = c("cluster_id" = clustering_method))
    }
    df[[clustering_method]] <- df$cluster_id
    qn <- t(SummarizedExperiment::assay(sce, "exprsQuantNorm"))
    df <- dplyr::bind_cols(df, as.data.frame(qn))
    df[!is.na(df$x) & !is.na(df$y), , drop = FALSE]
  }
  methods <- SingleCellExperiment::reducedDimNames(sce)
  frames <- stats::setNames(lapply(methods, build_dr_df), methods)
  set.seed(2)
  for (method in methods) {
    down <- frames[[method]] |>
      dplyr::group_by(sample_id) |>
      dplyr::group_modify(~ if (nrow(.x) > 20) .x[sample(nrow(.x), 20), ] else .x) |>
      dplyr::ungroup()
    frames[[paste0("Downsampled.", method)]] <- droplevels(down)
  }
  frames$All <- frames[[methods[1]]]
  frames$Downsampled <- frames[[paste0("Downsampled.", methods[1])]]
  frames
}

dr_test_sce <- function() {
  sce <- make_mock_sce()
  set.seed(3)
  SingleCellExperiment::reducedDim(sce, "TSNE") <- matrix(stats::rnorm(2 * ncol(sce)), ncol = 2)
  # An embedding run on a subset leaves the other cells without coordinates
  SingleCellExperiment::reducedDim(sce, "TSNE")[c(4, 50), ] <- NA
  sce$Tregs_UCell <- stats::runif(ncol(sce))
  clusters <- levels(sce$cluster_id)
  S4Vectors::metadata(sce)$cluster_codes <- data.frame(
    FlowSOM = factor(clusters, levels = clusters),
    meta3 = factor(rep_len(c("a", "b", "c"), length(clusters))))
  sce
}

test_that("plotting tables are not stored, and come back exactly as the pipeline built them", {
  skip_if_not_installed("anndataR")
  sce <- dr_test_sce()
  frames <- pipeline_dr_frames(sce, "FlowSOM")
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  env$umapDFList <- frames
  env$clusteringMethodToUse <- "FlowSOM"
  save_h5ad_data(tmp, envir = env)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  stored <- rhdf5::h5ls(h5ad_path, recursive = 2)
  rhdf5::h5closeAll()
  expect_true("dr_frames" %in% stored$name[stored$group == "/uns"])
  expect_false("dr_dataframes" %in% stored$name[stored$group == "/uns"])

  loaded <- new.env(); load_h5ad_to_env(h5ad_path, envir = loaded)
  for (back in list(loaded$umapDFList, load_h5ad_for_shiny(h5ad_path)$umapDFList)) {
    expect_identical(names(back), names(frames))
    for (n in names(frames)) {
      original <- as.data.frame(frames[[n]]); rownames(original) <- NULL
      expect_equal(as.data.frame(back[[n]]), original, tolerance = 0, ignore_attr = TRUE, info = n)
      expect_identical(lapply(back[[n]], class), lapply(original, class), info = n)
      expect_identical(lapply(back[[n]], levels), lapply(original, levels), info = n)
    }
  }
  expect_lt(nrow(loaded$umapDFList$TSNE), ncol(sce))            # cells without coordinates stay out
  expect_lt(nrow(loaded$umapDFList$Downsampled), ncol(sce))
})

test_that("a table that does not match the SCE is saved whole, not rebuilt wrongly", {
  skip_if_not_installed("anndataR")
  sce <- dr_test_sce()
  frames <- pipeline_dr_frames(sce, "FlowSOM")["UMAP"]
  frames$UMAP$x[1] <- frames$UMAP$x[1] + 1     # no longer what the SCE holds
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  env$umapDFList <- frames
  env$clusteringMethodToUse <- "FlowSOM"
  save_h5ad_data(tmp, envir = env)
  back <- load_h5ad_for_shiny(file.path(tmp, "marmot_results.h5ad"))$umapDFList
  expect_equal(back$UMAP$x, frames$UMAP$x, tolerance = 0)
})
