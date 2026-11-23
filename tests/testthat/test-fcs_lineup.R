# Write a small FCS file with the given channels and markers
write_test_fcs <- function(path, channels, markers, n = 50, seed = 1) {
  set.seed(seed)
  m <- matrix(runif(n * length(channels), 0, 1000), ncol = length(channels), dimnames = list(NULL, channels))
  pd <- data.frame(name = channels, desc = markers, range = 1024, minRange = 0, maxRange = 1023,
                   row.names = paste0("$P", seq_along(channels)), stringsAsFactors = FALSE)
  ff <- methods::new("flowFrame", exprs = m, parameters = Biobase::AnnotatedDataFrame(pd))
  suppressWarnings(flowCore::write.FCS(ff, path))
  path
}

test_that("files that spell, order and place markers differently are lined up", {
  skip_if_not_installed("flowCore")
  d <- withr::local_tempdir()
  f1 <- write_test_fcs(file.path(d, "a.fcs"), c("FSC-A", "BV421-A", "PE-A", "Time"), c(NA, "IL7R", "CD3", NA))
  # week two: different capitals, a hyphen, another order, IL7R on another channel, an extra channel
  f2 <- write_test_fcs(file.path(d, "b.fcs"), c("PE-A", "fsc-a", "BV605-A", "APC-A", "TIME"),
                       c("CD-3", NA, "Il7R", "CD19", NA), seed = 2)
  lu <- plan_fcs_lineup(c(f1, f2), sheet_markers = c("IL7R", "CD3"))

  expect_identical(lu$reference$channel, c("FSC-A", "BV421-A", "PE-A", "Time"))
  expect_identical(lu$reference$marker, c(NA, "IL7R", "CD3", NA))
  expect_identical(unname(lu$per_file[["b.fcs"]]), c("fsc-a", "BV605-A", "PE-A", "TIME"))
  expect_identical(nrow(lu$missing), 0L)
  expect_true(any(grepl("'Il7R' in 1 file(s) was taken as 'IL7R'", lu$notes, fixed = TRUE)))
  expect_true(any(grepl("'CD-3' in 1 file(s) was taken as 'CD3'", lu$notes, fixed = TRUE)))
  expect_true(any(grepl("'IL7R' is on BV421-A in 1 file(s) and BV605-A in 1 file(s)", lu$notes, fixed = TRUE)))
  expect_true(any(grepl("left out", lu$notes) & grepl("CD19", lu$notes)))

  # Lining up keeps every value and gives both files the same channels
  b <- flowCore::read.FCS(f2, transformation = FALSE, truncate_max_range = FALSE)
  b2 <- lineup_fcs_frame(b, lu, "b.fcs")
  a2 <- lineup_fcs_frame(flowCore::read.FCS(f1, transformation = FALSE, truncate_max_range = FALSE), lu, "a.fcs")
  expect_identical(flowCore::colnames(b2), flowCore::colnames(a2))
  expect_identical(unname(flowCore::exprs(b2)[, "BV421-A"]), unname(flowCore::exprs(b)[, "BV605-A"]))
  expect_identical(unname(flowCore::markernames(b2)[["BV421-A"]]), "IL7R")
  fs <- methods::as(list(a.fcs = a2, b.fcs = b2), "flowSet")
  expect_s4_class(fs, "flowSet")
})

test_that("a sheet marker missing from a file is reported", {
  skip_if_not_installed("flowCore")
  d <- withr::local_tempdir()
  f1 <- write_test_fcs(file.path(d, "a.fcs"), c("BV421-A", "PE-A"), c("IL7R", "CD3"))
  f2 <- write_test_fcs(file.path(d, "b.fcs"), c("PE-A"), c("CD3"))
  lu <- plan_fcs_lineup(c(f1, f2), sheet_markers = c("IL7R", "CD3"))
  expect_identical(lu$missing$marker, "IL7R")
  expect_identical(lu$missing$files, "b.fcs")
})

test_that("identical files need no notes", {
  skip_if_not_installed("flowCore")
  d <- withr::local_tempdir()
  f1 <- write_test_fcs(file.path(d, "a.fcs"), c("FSC-A", "PE-A"), c(NA, "CD3"))
  f2 <- write_test_fcs(file.path(d, "b.fcs"), c("FSC-A", "PE-A"), c(NA, "CD3"), seed = 2)
  lu <- plan_fcs_lineup(c(f1, f2), sheet_markers = "CD3")
  expect_identical(lu$notes, character())
})
