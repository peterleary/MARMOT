# A results folder saved in format 2 and kept in the repository. Every later
# MARMOT 2.x must still read it: results made by 2.0 onwards stay loadable.
# See fixtures/make_format2_fixture.R. Do not regenerate the folder to make
# these tests pass.

fixture_dir <- function() testthat::test_path("fixtures", "format2")

test_that("the saved format 2 folder passes the format check", {
  h5ad_path <- file.path(fixture_dir(), "R_files", "marmot_results.h5ad")
  expect_true(file.exists(h5ad_path))
  expect_equal(check_results_format(h5ad_path)$format_version, 2L)
})

test_that("the saved format 2 folder loads back to the values it was made from", {
  skip_if_not_installed("anndataR")
  h5ad_path <- file.path(fixture_dir(), "R_files", "marmot_results.h5ad")
  expected <- readRDS(file.path(fixture_dir(), "expected.rds"))
  plain <- function(m) unname(as.matrix(m))

  report <- new.env()
  suppressWarnings(load_h5ad_to_env(h5ad_path, envir = report))
  explorer <- suppressWarnings(load_h5ad_for_shiny(h5ad_path))

  for (sce in list(report$sce, explorer$sce, reconstruct_sce_from_h5ad(h5ad_path))) {
    expect_identical(SummarizedExperiment::assayNames(sce), names(expected$assays))
    for (a in names(expected$assays)) {
      expect_identical(plain(SummarizedExperiment::assay(sce, a)), expected$assays[[a]], info = a)
    }
    expect_identical(rownames(sce), expected$markers)
    expect_identical(SingleCellExperiment::reducedDimNames(sce), names(expected$embeddings))
    for (e in names(expected$embeddings)) {
      expect_identical(plain(SingleCellExperiment::reducedDim(sce, e)), expected$embeddings[[e]], info = e)
    }
    expect_identical(event_numbers(sce), expected$event_numbers)
    expect_false("event_number" %in% colnames(SummarizedExperiment::colData(sce)))
    expect_identical(as.character(sce$sample_id), expected$sample_id)
    expect_identical(as.character(sce$condition), expected$condition)
    expect_identical(as.character(sce$cluster_id), expected$cluster_id)
  }

  for (loaded in list(as.list(report), explorer)) {
    expect_identical(names(loaded$daList), names(expected$daList))
    expect_identical(names(loaded$dsList), names(expected$dsList))
    for (i in seq_along(expected$daList)) {
      expect_equal(loaded$daList[[i]]$logFC, expected$daList[[i]]$logFC)
      expect_equal(loaded$dsList[[i]]$logFC, expected$dsList[[i]]$logFC)
      expect_equal(as.character(loaded$daList[[i]]$cluster_id), expected$daList[[i]]$cluster_id)
    }
    expect_equal(as.character(loaded$selectedClustersList[["Contrast B+ over A: Up"]]), expected$selected_up)
    expect_identical(as.character(loaded$sourceFiles$file_name), expected$sourceFiles$file_name)
    expect_identical(as.character(loaded$sourceFiles$md5), expected$sourceFiles$md5)
    expect_equal(as.integer(loaded$sourceFiles$events), expected$sourceFiles$events)
  }
  expect_equal(report$kValuesIWant, expected$kValuesIWant)
  expect_equal(report$clusteringMethodToUse, "FlowSOM")
})
