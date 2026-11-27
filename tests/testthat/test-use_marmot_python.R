test_that("the pipeline's Python check finds what the PARC call finds (pixi too)", {
  reset <- function() {
    e <- MARMOT:::.marmot_py
    e$bound <- FALSE; e$available <- FALSE; e$path <- NULL
    e$subprocess_checked <- NULL; e$subprocess_path <- NULL
  }
  reset(); withr::defer(reset())
  # A pixi environment, no conda: PARC must not fall back to Mparc
  local_mocked_bindings(.find_p4r_python = function() "/opt/marmot-data/python/.pixi/envs/default/bin/python")
  expect_true(use_marmot_python())
  expect_identical(MARMOT:::.marmot_py$path, "/opt/marmot-data/python/.pixi/envs/default/bin/python")
})

test_that("with no Python anywhere, the check says so", {
  reset <- function() {
    e <- MARMOT:::.marmot_py
    e$bound <- FALSE; e$available <- FALSE; e$path <- NULL
    e$subprocess_checked <- NULL; e$subprocess_path <- NULL
  }
  reset(); withr::defer(reset())
  local_mocked_bindings(.find_p4r_python = function() NULL)
  local_mocked_bindings(py_available = function(...) FALSE, .package = "reticulate")
  local_mocked_bindings(use_condaenv = function(...) stop("no conda"), .package = "reticulate")
  expect_false(suppressWarnings(use_marmot_python()))
})
