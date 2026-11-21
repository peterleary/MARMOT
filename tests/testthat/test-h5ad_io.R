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
  expect_equal(ad$uns$marmot_manifest$format, "marmot-results")
  expect_equal(as.integer(ad$uns$marmot_manifest$schema_version), 2L)
  expect_true(file.exists(file.path(tmp, "marmot_format.json")))
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

test_that("round-trip: a QC table with '/' in a column name survives", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()

  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$smd <- data.frame(study = "test", stringsAsFactors = FALSE)
  env_save$umapDFList <- list(All = make_mock_umap_df(sce))
  env_save$coloursList <- make_mock_colours(sce)
  # Column names as PeacoQC writes them
  env_save$QCmini <- data.frame(
    Filename = c("a.fcs", "b.fcs"),
    `% Full analysis` = c(1.5, 2.5),
    `Increasing/Decreasing channel` = c("No", "Yes"),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  save_h5ad_data(tmp, envir = env_save)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  env_load <- new.env(parent = emptyenv())
  expect_no_warning(load_h5ad_to_env(h5ad_path, envir = env_load), message = "qc_summary")
  loaded <- get("QCmini", envir = env_load)
  expect_equal(names(loaded), names(env_save$QCmini))
  expect_equal(as.character(loaded$`Increasing/Decreasing channel`), c("No", "Yes"))
  expect_equal(as.numeric(loaded$`% Full analysis`), c(1.5, 2.5))
})

test_that("round-trip: contrasts whose names collapse to the same key are all kept, with their real names", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  tmp <- withr::local_tempdir()
  env_save <- new.env(parent = emptyenv())
  env_save$sce <- sce
  env_save$md <- S4Vectors::metadata(sce)$experiment_info
  env_save$smd <- data.frame(study = "test", stringsAsFactors = FALSE)
  env_save$umapDFList <- list(All = make_mock_umap_df(sce))
  env_save$coloursList <- make_mock_colours(sce)
  tab <- function(v) data.frame(cluster_id = c("1", "2"), logFC = c(v, -v), p_adj = c(0.01, 0.5))
  # "+" and "-" both become "_" in the stored key; Z sorts last but was saved first
  env_save$daList <- list("Z+ over B" = tab(3), "A+ over B" = tab(1), "A- over B" = tab(2))
  env_save$dsList <- list("A+ over B" = list(tbl_DS = tab(4)), "A- over B" = tab(5))

  save_h5ad_data(tmp, envir = env_save)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  env_load <- new.env(parent = emptyenv())
  suppressWarnings(load_h5ad_to_env(h5ad_path, envir = env_load))
  da <- get("daList", envir = env_load)
  expect_equal(names(da), c("Z+ over B", "A+ over B", "A- over B"))
  expect_equal(da[["A+ over B"]]$logFC, c(1, -1))
  expect_equal(da[["A- over B"]]$logFC, c(2, -2))
  ds <- get("dsList", envir = env_load)
  expect_equal(names(ds), c("A+ over B", "A- over B"))
  expect_equal(ds[["A- over B"]]$logFC, c(5, -5))

  shiny <- suppressWarnings(load_h5ad_for_shiny(h5ad_path))
  expect_equal(names(shiny$daList), c("Z+ over B", "A+ over B", "A- over B"))
  expect_equal(shiny$daList[["A- over B"]]$logFC, c(2, -2))
})

test_that("round-trip: the original FCS files are recorded with checksum and event count", {
  skip_if_not_installed("anndataR")
  d <- withr::local_tempdir()
  paths <- file.path(d, c("b.fcs", "a.fcs"))
  suppressWarnings(flowCore::write.FCS(flowCore::flowFrame(cbind(A = 1:30, B = 30:1)), paths[1]))
  suppressWarnings(flowCore::write.FCS(flowCore::flowFrame(cbind(A = 1:7, B = 7:1)), paths[2]))

  sce <- make_mock_sce()
  out <- file.path(d, "R_files"); dir.create(out)
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  env$fcs_files <- paths
  save_h5ad_data(out, envir = env)
  h5ad_path <- file.path(out, "marmot_results.h5ad")

  for (source_files in list(load_h5ad_for_shiny(h5ad_path)$sourceFiles,
                            { e <- new.env(); load_h5ad_to_env(h5ad_path, envir = e); e$sourceFiles })) {
    expect_identical(as.character(source_files$file_name), c("b.fcs", "a.fcs"))
    expect_identical(as.character(source_files$md5), unname(tools::md5sum(paths)))
    expect_equal(as.integer(source_files$events), c(30L, 7L))
  }

  # Saved again from reloaded results, where the FCS paths are gone, it is kept
  e <- new.env(); load_h5ad_to_env(h5ad_path, envir = e)
  out2 <- file.path(d, "again"); dir.create(out2)
  save_h5ad_data(out2, envir = e)
  expect_identical(as.character(load_h5ad_for_shiny(file.path(out2, "marmot_results.h5ad"))$sourceFiles$md5),
                   unname(tools::md5sum(paths)))
})

test_that("the arcsinh matrix is stored once, as X, and every assay comes back as it was", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  # As in the pipeline: three names for one matrix, plus two derived layers
  main <- SummarizedExperiment::assay(sce, "exprsTransformed")
  SummarizedExperiment::assays(sce) <- list(
    counts = main, exprs = main, exprsTransformed = main,
    exprsQuantNorm = SummarizedExperiment::assay(sce, "exprsQuantNorm"), norm = main * 2)
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  save_h5ad_data(tmp, envir = env)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  stored <- rhdf5::h5ls(h5ad_path, recursive = 2)
  rhdf5::h5closeAll()
  expect_true("X" %in% stored$name[stored$group == "/"])
  expect_setequal(stored$name[stored$group == "/layers"], c("exprsQuantNorm", "norm"))

  back <- reconstruct_sce_from_h5ad(h5ad_path)
  expect_identical(SummarizedExperiment::assayNames(back), SummarizedExperiment::assayNames(sce))
  for (a in SummarizedExperiment::assayNames(sce)) {
    expect_identical(unname(as.matrix(SummarizedExperiment::assay(back, a))),
                     unname(as.matrix(SummarizedExperiment::assay(sce, a))), info = a)
    expect_identical(dimnames(SummarizedExperiment::assay(back, a)), dimnames(SummarizedExperiment::assay(sce, a)), info = a)
  }
  expect_identical(SummarizedExperiment::assayNames(load_h5ad_for_shiny(h5ad_path)$sce),
                   SummarizedExperiment::assayNames(sce))
})

test_that("an assay that differs from the main matrix by one value keeps its own layer", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  main <- SummarizedExperiment::assay(sce, "exprsTransformed")
  nearly <- main; nearly[1, 1] <- nearly[1, 1] + 1e-12
  SummarizedExperiment::assays(sce) <- list(counts = nearly, exprs = main, exprsTransformed = main)
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  save_h5ad_data(tmp, envir = env)
  back <- reconstruct_sce_from_h5ad(file.path(tmp, "marmot_results.h5ad"))
  expect_identical(unname(as.matrix(SummarizedExperiment::assay(back, "counts"))), unname(as.matrix(nearly)))
  expect_identical(unname(as.matrix(SummarizedExperiment::assay(back, "exprs"))), unname(as.matrix(main)))
})

test_that("round-trip: a panel with FCS-style 'AsIs' columns reads back, without warnings", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  # As the pipeline builds it: columns taken from flowCore's parameter table
  env$panel <- data.frame(fcs_colname = I(c("FJComp-A", "FJComp-B")), marker_name = I(c("CD4", "CD8")),
                          marker_class = c("type", "state"), stringsAsFactors = FALSE)
  save_h5ad_data(tmp, envir = env)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  loaded <- new.env()
  expect_no_warning(load_h5ad_to_env(h5ad_path, envir = loaded))
  expect_identical(as.character(loaded$panel$fcs_colname), c("FJComp-A", "FJComp-B"))
  expect_identical(as.character(loaded$panel$marker_name), c("CD4", "CD8"))
  expect_no_warning(load_h5ad_for_shiny(h5ad_path))
})

test_that("embeddings are stored under scanpy's names and come back under MARMOT's", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  n <- ncol(sce)
  set.seed(1)
  embeddings <- list(PCA = matrix(rnorm(n * 3), ncol = 3), UMAP = matrix(rnorm(n * 2), ncol = 2),
                     TSNE = matrix(rnorm(n * 2), ncol = 2), Mpacmap = matrix(rnorm(n * 2), ncol = 2))
  SingleCellExperiment::reducedDims(sce) <- embeddings
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  save_h5ad_data(tmp, envir = env)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  stored <- rhdf5::h5ls(h5ad_path, recursive = 2)
  rhdf5::h5closeAll()
  expect_setequal(stored$name[stored$group == "/obsm"], c("X_pca", "X_umap", "X_tsne", "X_mpacmap"))

  for (back in list(reconstruct_sce_from_h5ad(h5ad_path), load_h5ad_for_shiny(h5ad_path)$sce)) {
    expect_identical(SingleCellExperiment::reducedDimNames(back), names(embeddings))
    for (e in names(embeddings)) {
      expect_identical(unname(as.matrix(SingleCellExperiment::reducedDim(back, e))), unname(embeddings[[e]]), info = e)
    }
  }
  loaded <- new.env()
  load_h5ad_to_env(h5ad_path, envir = loaded)
  expect_identical(SingleCellExperiment::reducedDimNames(loaded$sce), names(embeddings))
})

test_that("embedding names differing only in case do not collide when stored", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  n <- ncol(sce)
  embeddings <- list(PaCMAP = matrix(1, n, 2), pacmap = matrix(2, n, 2))
  SingleCellExperiment::reducedDims(sce) <- embeddings
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  save_h5ad_data(tmp, envir = env)
  back <- reconstruct_sce_from_h5ad(file.path(tmp, "marmot_results.h5ad"))
  expect_identical(SingleCellExperiment::reducedDimNames(back), names(embeddings))
  expect_equal(unname(as.matrix(SingleCellExperiment::reducedDim(back, "pacmap")))[1, 1], 2)
})
