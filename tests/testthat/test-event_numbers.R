# Tests for R/event_numbers.R

make_numbered_frame <- function(n, offset) {
  # Channel A holds a value unique to the file and the event, so any mix-up shows.
  m <- cbind(A = offset + seq_len(n), B = stats::runif(n))
  ff <- flowCore::flowFrame(m)
  # As the pipeline does: tells CATALYST these are flow, not mass, channels
  flowCore::keyword(ff)[["$CYT"]] <- "FACS"
  ff
}

test_that("events are numbered from 1 in file order, and the numbers follow the cells", {
  ff <- add_event_numbers(make_numbered_frame(50, 1000))
  expect_identical(as.integer(flowCore::exprs(ff)[, "MARMOT_event"]), 1:50)

  picked <- c(40L, 3L, 17L)
  sub <- ff[picked, ]
  expect_identical(as.integer(flowCore::exprs(sub)[, "MARMOT_event"]), picked)
  expect_identical(flowCore::exprs(sub)[, "A"], 1000 + picked)
})

test_that("numbering twice does not add a second column", {
  ff <- add_event_numbers(add_event_numbers(make_numbered_frame(10, 0)))
  expect_identical(sum(flowCore::colnames(ff) == "MARMOT_event"), 1L)
})

test_that("event numbers survive being written to an FCS file and read back", {
  ff <- add_event_numbers(make_numbered_frame(2000, 0))[seq(2000, 1, by = -7), ]
  path <- file.path(withr::local_tempdir(), "kept.fcs")
  suppressWarnings(flowCore::write.FCS(ff, path))
  back <- flowCore::read.FCS(path, transformation = FALSE, truncate_max_range = FALSE)
  expect_identical(as.integer(flowCore::exprs(back)[, "MARMOT_event"]), seq(2000L, 1L, by = -7L))
})

test_that("each cell of the SCE gets the event number of the event it came from", {
  skip_if_not_installed("CATALYST")
  set.seed(1)
  files <- c("one.fcs", "two.fcs", "three.fcs")
  offsets <- c(10000, 20000, 30000)
  # Sample each file differently, in a shuffled order, as the pipeline does
  frames <- lapply(seq_along(files), function(i) {
    ff <- add_event_numbers(make_numbered_frame(300, offsets[i]))
    ff[sample(300, 100 + 20 * i), ]
  })
  names(frames) <- files
  fs <- methods::as(frames, "flowSet")
  flowCore::sampleNames(fs) <- files

  # Metadata lists the files in a different order from the flowSet
  md <- data.frame(file_name = rev(files), sample_id = c("S3", "S2", "S1"),
                   condition = c("x", "y", "x"), stringsAsFactors = FALSE)
  panel <- data.frame(fcs_colname = c("A", "B"), antigen = c("A", "B"),
                      marker_class = "type", stringsAsFactors = FALSE)
  sce <- suppressWarnings(suppressMessages(CATALYST::prepData(
    fs, panel = panel, md = md, features = panel$fcs_colname, transform = FALSE,
    md_cols = list(factors = "condition"))))

  sce <- record_event_numbers(sce, fs, md)
  numbers <- event_numbers(sce)
  expect_length(numbers, ncol(sce))
  expect_false("event_number" %in% colnames(SummarizedExperiment::colData(sce)))

  # They follow the cells when the SCE is subset
  some <- c(5L, 200L, 17L)
  expect_identical(event_numbers(sce[, some]), numbers[some])

  # Channel A is offset + event number, so it gives the right answer directly
  a <- SummarizedExperiment::assay(sce, "counts")[1, ]
  file_offset <- offsets[match(md$file_name[match(as.character(sce$sample_id), md$sample_id)], files)]
  expect_equal(unname(a), file_offset + numbers)
})

test_that("a frame without event numbers, or with the wrong number of cells, is an error", {
  skip_if_not_installed("CATALYST")
  files <- c("one.fcs", "two.fcs")
  frames <- list(add_event_numbers(make_numbered_frame(20, 0)), add_event_numbers(make_numbered_frame(30, 100)))
  names(frames) <- files
  fs <- methods::as(frames, "flowSet"); flowCore::sampleNames(fs) <- files
  md <- data.frame(file_name = files, sample_id = c("S1", "S2"), condition = c("x", "y"))
  panel <- data.frame(fcs_colname = c("A", "B"), antigen = c("A", "B"), marker_class = "type")
  sce <- suppressWarnings(suppressMessages(CATALYST::prepData(
    fs, panel = panel, md = md, features = panel$fcs_colname, transform = FALSE,
    md_cols = list(factors = "condition"))))

  expect_error(record_event_numbers(sce[, -1], fs, md), "Cannot match")

  bare <- lapply(frames, function(f) f[, c("A", "B")])
  fs_bare <- methods::as(bare, "flowSet"); flowCore::sampleNames(fs_bare) <- files
  expect_error(record_event_numbers(sce, fs_bare, md), "No event numbers")
})

test_that("event numbers survive saving and loading, and stay out of the cell table", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  numbers <- sample.int(100000L, ncol(sce))
  SingleCellExperiment::int_colData(sce)$event_number <- numbers
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  env$umapDFList <- list(All = make_mock_umap_df(sce))
  env$coloursList <- make_mock_colours(sce)
  save_h5ad_data(tmp, envir = env)

  back <- reconstruct_sce_from_h5ad(file.path(tmp, "marmot_results.h5ad"))
  expect_identical(event_numbers(back), numbers)
  expect_false("event_number" %in% colnames(SummarizedExperiment::colData(back)))
  expect_identical(event_numbers(load_h5ad_for_shiny(file.path(tmp, "marmot_results.h5ad"))$sce), numbers)
})

test_that("results without event numbers load as before", {
  skip_if_not_installed("anndataR")
  sce <- make_mock_sce()
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  save_h5ad_data(tmp, envir = env)
  expect_null(event_numbers(reconstruct_sce_from_h5ad(file.path(tmp, "marmot_results.h5ad"))))
})
