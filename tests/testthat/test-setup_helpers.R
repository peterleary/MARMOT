test_that("the dependency list is read from DESCRIPTION, without base or developer packages", {
  deps <- MARMOT:::.marmot_dependencies(list(
    Imports = "CATALYST, stats, flowCore (>= 2.0)",
    Suggests = "harmony, testthat (>= 3.0.0), withr, Rphenograph"
  ))
  expect_identical(deps$imports, c("CATALYST", "flowCore"))
  expect_identical(deps$suggests, c("harmony", "Rphenograph"))
})

test_that("MARMOT's own list includes the methods and features it calls", {
  deps <- MARMOT:::.marmot_dependencies()
  expect_true(all(c("harmony", "Rphenograph", "PeacoQC", "flowAI") %in% deps$suggests))
  expect_true(all(c("CATALYST", "diffcyt", "RANN", "RcppHNSW") %in% deps$imports))
})

test_that("check_setup reports every package it checked", {
  out <- capture.output(res <- check_setup())
  expect_true(all(c("package", "type", "status", "version") %in% names(res)))
  expect_true(any(grepl("run the pipeline", out)))
})

test_that("the pixi environment files ship with MARMOT", {
  expect_true(nzchar(system.file("python", "pixi.toml", package = "MARMOT")))
  expect_true(nzchar(system.file("python", "pixi.lock", package = "MARMOT")))
  lock <- readLines(system.file("python", "pixi.lock", package = "MARMOT"))
  for (p in c("osx-arm64:", "osx-64:", "linux-64:", "win-64:")) expect_true(any(grepl(p, lock, fixed = TRUE)), info = p)
})
