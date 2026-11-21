# Tests for R/slim_sce.R

test_that("a slimmed SCE has no expression data, and gets exactly its own back", {
  sce <- make_mock_sce()
  sce_ds <- sce
  SummarizedExperiment::rowData(sce_ds)$marker_class <- "state"     # as the pipeline does
  S4Vectors::metadata(sce_ds)$extra <- "kept"

  slim <- slim_sce(sce_ds)
  expect_length(SummarizedExperiment::assayNames(slim), 0)
  expect_length(SingleCellExperiment::reducedDimNames(slim), 0)
  expect_lt(as.numeric(utils::object.size(slim)), as.numeric(utils::object.size(sce_ds)))

  # `full` may hold more than the slimmed object ever had
  full <- sce
  SingleCellExperiment::reducedDim(full, "Later") <- matrix(0, ncol(full), 2)
  back <- restore_slim_sce(slim, full)
  expect_identical(SummarizedExperiment::assayNames(back), SummarizedExperiment::assayNames(sce_ds))
  for (a in SummarizedExperiment::assayNames(sce_ds)) {
    expect_identical(SummarizedExperiment::assay(back, a), SummarizedExperiment::assay(sce_ds, a), info = a)
  }
  expect_identical(SingleCellExperiment::reducedDimNames(back), SingleCellExperiment::reducedDimNames(sce_ds))
  expect_identical(as.character(SummarizedExperiment::rowData(back)$marker_class), rep("state", nrow(sce)))
  expect_identical(S4Vectors::metadata(back), S4Vectors::metadata(sce_ds))
})

test_that("restoring from different cells is refused", {
  sce <- make_mock_sce()
  slim <- slim_sce(sce)
  expect_error(restore_slim_sce(slim, sce[, -1]), "same cells")
  expect_error(restore_slim_sce(slim, sce[, rev(seq_len(ncol(sce)))]), "same cells")
  expect_error(restore_slim_sce(slim, sce[-1, ]), "same cells")
})

test_that("an SCE that was saved whole, or NULL, passes through", {
  sce <- make_mock_sce()
  expect_identical(restore_slim_sce(sce, sce), sce)
  expect_null(slim_sce(NULL))
  expect_null(restore_slim_sce(NULL, sce))
})
