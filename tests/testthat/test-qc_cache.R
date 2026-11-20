# Tests for R/qc_cache.R

make_fake_fcs <- function(dir, name, content) {
  path <- file.path(dir, name)
  writeLines(content, path)
  path
}

test_that("a stamp that was written is current, and a missing one is not", {
  d <- withr::local_tempdir()
  qc_dir <- file.path(d, "qc"); dir.create(qc_dir)
  files <- c(make_fake_fcs(d, "a.fcs", "one"), make_fake_fcs(d, "b.fcs", "two"))
  stamp <- qc_cache_stamp(files, "PeacoQC", list(cofactors = list(X = 2500)))

  expect_false(qc_cache_is_current(qc_dir, stamp))   # no stamp yet (as from 1.x)
  qc_cache_write_stamp(qc_dir, stamp)
  expect_true(qc_cache_is_current(qc_dir, stamp))
  expect_true(qc_cache_is_current(qc_dir, qc_cache_stamp(files, "PeacoQC", list(cofactors = list(X = 2500)))))
})

test_that("changed file content with the same name invalidates the cache", {
  d <- withr::local_tempdir()
  qc_dir <- file.path(d, "qc"); dir.create(qc_dir)
  files <- c(make_fake_fcs(d, "a.fcs", "one"), make_fake_fcs(d, "b.fcs", "two"))
  qc_cache_write_stamp(qc_dir, qc_cache_stamp(files, "FlowAI"))

  make_fake_fcs(d, "b.fcs", "two, re-exported")
  expect_false(qc_cache_is_current(qc_dir, qc_cache_stamp(files, "FlowAI")))
})

test_that("a changed cofactor, tool, or set of files invalidates the cache", {
  d <- withr::local_tempdir()
  qc_dir <- file.path(d, "qc"); dir.create(qc_dir)
  files <- c(make_fake_fcs(d, "a.fcs", "one"), make_fake_fcs(d, "b.fcs", "two"))
  qc_cache_write_stamp(qc_dir, qc_cache_stamp(files, "PeacoQC", list(cofactors = list(X = 2500, Y = 500))))

  expect_false(qc_cache_is_current(qc_dir,
    qc_cache_stamp(files, "PeacoQC", list(cofactors = list(X = 250, Y = 500)))))
  expect_false(qc_cache_is_current(qc_dir,
    qc_cache_stamp(files, "FlowAI", list(cofactors = list(X = 2500, Y = 500)))))
  expect_false(qc_cache_is_current(qc_dir,
    qc_cache_stamp(files[1], "PeacoQC", list(cofactors = list(X = 2500, Y = 500)))))
})

test_that("the order the files are given in does not matter", {
  d <- withr::local_tempdir()
  files <- c(make_fake_fcs(d, "a.fcs", "one"), make_fake_fcs(d, "b.fcs", "two"))
  expect_identical(qc_cache_stamp(files, "FlowAI"), qc_cache_stamp(rev(files), "FlowAI"))
})

test_that("a missing input file is an error, not a silent stamp", {
  d <- withr::local_tempdir()
  expect_error(qc_cache_stamp(file.path(d, "nope.fcs"), "FlowAI"), "not found")
})
