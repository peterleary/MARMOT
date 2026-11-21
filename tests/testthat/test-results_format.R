# Tests for R/results_format.R

save_mock_results <- function(dir) {
  sce <- make_mock_sce()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  env$smd <- data.frame(study = "test", stringsAsFactors = FALSE)
  env$umapDFList <- list(All = make_mock_umap_df(sce))
  env$coloursList <- make_mock_colours(sce)
  save_h5ad_data(dir, envir = env)
  file.path(dir, "marmot_results.h5ad")
}

# Turn freshly saved results into what MARMOT 1.3.2 wrote: no JSON stamp, and
# format version 1 inside the h5ad.
make_it_look_like_1x <- function(h5ad_path) {
  unlink(file.path(dirname(h5ad_path), "marmot_format.json"))
  rhdf5::h5write(1L, h5ad_path, "uns/marmot_manifest/schema_version")
  rhdf5::h5closeAll()
}

test_that("fresh results are stamped as format 2 and pass the check", {
  skip_if_not_installed("anndataR")
  h5ad_path <- save_mock_results(withr::local_tempdir())

  found <- read_results_format(h5ad_path)
  expect_identical(found$format_version, 2L)
  expect_identical(found$marmot_version, as.character(utils::packageVersion("MARMOT")))
  expect_no_error(check_results_format(h5ad_path))
})

test_that("a lone h5ad, without the JSON beside it, is still recognised", {
  skip_if_not_installed("anndataR")
  h5ad_path <- save_mock_results(withr::local_tempdir())
  unlink(file.path(dirname(h5ad_path), "marmot_format.json"))

  expect_identical(read_results_format(h5ad_path)$format_version, 2L)
  expect_no_error(check_results_format(h5ad_path))
})

test_that("results from MARMOT 1.x are refused by every loader, with the 1.x message", {
  skip_if_not_installed("anndataR")
  h5ad_path <- save_mock_results(withr::local_tempdir())
  make_it_look_like_1x(h5ad_path)

  expect_identical(read_results_format(h5ad_path)$format_version, 1L)
  expect_error(check_results_format(h5ad_path), "old marmots")
  expect_error(load_h5ad_to_env(h5ad_path, envir = new.env()), "old marmots")
  expect_error(load_h5ad_for_shiny(h5ad_path), "old marmots")
  expect_error(reconstruct_sce_from_h5ad(h5ad_path), "old marmots")
})

test_that("a file with no stamp at all is treated as 1.x", {
  d <- withr::local_tempdir()
  h5ad_path <- file.path(d, "marmot_results.h5ad")
  rhdf5::h5createFile(h5ad_path)
  rhdf5::h5closeAll()

  expect_true(is.na(read_results_format(h5ad_path)$format_version))
  expect_error(check_results_format(h5ad_path), "old marmots")
})

test_that("results from a newer MARMOT are refused, naming both versions", {
  skip_if_not_installed("anndataR")
  h5ad_path <- save_mock_results(withr::local_tempdir())
  write_results_format(dirname(h5ad_path), list(
    format = "marmot-results", format_version = 3L,
    marmot_version = "3.1.0", timestamp = "2030-01-01T00:00:00"
  ))

  err <- expect_error(check_results_format(h5ad_path), "newer marmots")
  expect_match(conditionMessage(err), "MARMOT 3.1.0", fixed = TRUE)
  expect_match(conditionMessage(err), as.character(utils::packageVersion("MARMOT")), fixed = TRUE)
  expect_error(load_h5ad_for_shiny(h5ad_path), "newer marmots")
})

test_that("format errors carry their own class, so the Explorer can show them plainly", {
  d <- withr::local_tempdir()
  h5ad_path <- file.path(d, "marmot_results.h5ad")
  file.create(h5ad_path)
  write_results_format(d, list(format = "marmot-results", format_version = 1L, marmot_version = "1.3.2"))
  err <- tryCatch(check_results_format(h5ad_path), error = function(e) e)
  expect_s3_class(err, "marmot_results_format")
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "old marmots")
})
