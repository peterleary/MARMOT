test_that("the run log records settings, sections, checkpoints and the finish", {
  path <- file.path(withr::local_tempdir(), "R_files", "marmot_run_log.json")

  run_log_start(path, settings = list(knn = 25, useQC = TRUE))
  expect_true(file.exists(path))
  log <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(log$settings$knn, 25)
  expect_equal(log$render$status, "in_progress")
  expect_false("crash_chunk" %in% names(log$render))

  run_log_record(path, "data_shape", list(n_samples = 4))
  run_log_checkpoint(path, "analysis_metadata")
  run_log_finish(path)

  log <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(log$data_shape$n_samples, 4)
  expect_equal(log$render$last_completed_chunk, "analysis_metadata")
  expect_equal(log$render$status, "success")
  expect_true(is.numeric(log$render$duration_seconds))
})

test_that("the run log does nothing until it has been started", {
  path <- file.path(withr::local_tempdir(), "marmot_run_log.json")
  run_log_record(path, "data_shape", list(n_samples = 4))
  run_log_checkpoint(path, "analysis_metadata")
  run_log_finish(path)
  expect_false(file.exists(path))
})
