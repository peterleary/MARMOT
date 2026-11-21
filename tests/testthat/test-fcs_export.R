# Tests for R/fcs_export.R

make_fcs <- function(dir, name, n = 60, offset = 0, seed = 1) {
  set.seed(seed)
  m <- cbind(FSC = offset + seq_len(n), `CD3 BV421` = stats::runif(n, 0, 1e5),
             CD4 = stats::rnorm(n))
  ff <- flowCore::flowFrame(m)
  flowCore::keyword(ff)[["$CYT"]] <- "FACS"
  path <- file.path(dir, name)
  suppressWarnings(flowCore::write.FCS(ff, path))
  path
}

read_raw <- function(path) {
  flowCore::read.FCS(path, transformation = FALSE, truncate_max_range = FALSE)
}

test_that("exported cells equal the chosen original rows, in every channel", {
  dir <- withr::local_tempdir(); out <- withr::local_tempdir()
  p <- make_fcs(dir, "a.fcs")
  ev <- c(50L, 3L, 17L, 4L)
  cells <- data.frame(file_name = "a.fcs", sample_id = "S1", event_number = ev,
                      umap_x = ev * 10, umap_y = -ev, cluster_id_codes = ev %% 3)
  written <- write_annotated_fcs(cells, dir, out)
  expect_equal(basename(written), "S1_modified.fcs")
  orig <- flowCore::exprs(read_raw(p))
  back <- flowCore::exprs(read_raw(written))
  expect_equal(nrow(back), 4L)
  keep <- sort(ev)
  for (ch in colnames(orig)) expect_equal(unname(back[, ch]), unname(orig[keep, ch]))
})

test_that("appended columns land on the right cells, whatever the input order", {
  dir <- withr::local_tempdir(); out <- withr::local_tempdir()
  make_fcs(dir, "a.fcs")
  ev <- c(40L, 2L, 25L, 11L, 33L)
  cells <- data.frame(file_name = "a.fcs", sample_id = "S1", event_number = ev,
                      umap_x = ev * 2, cluster_id_codes = ev + 100)
  written <- write_annotated_fcs(cells, dir, out)
  back <- flowCore::exprs(read_raw(written))
  expect_equal(unname(back[, "FSC"]), as.numeric(sort(ev)))   # FSC = event number
  expect_equal(unname(back[, "umap_x"]), sort(ev) * 2)
  expect_equal(unname(back[, "cluster_id_codes"]), sort(ev) + 100)
})

test_that("only the chosen cells are exported, one file per sample", {
  dir <- withr::local_tempdir(); out <- withr::local_tempdir()
  make_fcs(dir, "a.fcs", n = 100); make_fcs(dir, "b.fcs", n = 80, offset = 1000)
  cells <- data.frame(file_name = rep(c("a.fcs", "b.fcs"), c(5, 7)),
                      sample_id = rep(c("A", "B"), c(5, 7)),
                      event_number = c(1:5, 10:16), code = 1)
  written <- write_annotated_fcs(cells, dir, out)
  expect_equal(sort(basename(written)), c("A_modified.fcs", "B_modified.fcs"))
  expect_equal(nrow(read_raw(file.path(out, "A_modified.fcs"))), 5L)
  expect_equal(nrow(read_raw(file.path(out, "B_modified.fcs"))), 7L)
})

test_that("appended columns are linear in the written file", {
  dir <- withr::local_tempdir(); out <- withr::local_tempdir()
  make_fcs(dir, "a.fcs")
  cells <- data.frame(file_name = "a.fcs", sample_id = "S1", event_number = 1:5,
                      umap_x = rnorm(5), cluster_id_codes = 1)
  written <- write_annotated_fcs(cells, dir, out)
  kw <- flowCore::keyword(read_raw(written))
  idx <- which(unlist(lapply(1:5, function(i) kw[[paste0("$P", i, "N")]])) %in%
                 c("umap_x", "cluster_id_codes"))
  expect_length(idx, 2L)
  for (i in idx) expect_equal(kw[[paste0("$P", i, "E")]], "0,0")
})

test_that("a missing file gives a classed error naming the file and folder", {
  dir <- withr::local_tempdir(); out <- withr::local_tempdir()
  cells <- data.frame(file_name = "gone.fcs", sample_id = "S1", event_number = 1L, code = 1)
  err <- expect_error(write_annotated_fcs(cells, dir, out), class = "marmot_fcs_not_found")
  expect_match(conditionMessage(err), "gone.fcs", fixed = TRUE)
  expect_match(conditionMessage(err), dir, fixed = TRUE)
})

test_that("a changed file is refused, an unchanged one is accepted", {
  dir <- withr::local_tempdir(); out <- withr::local_tempdir()
  p <- make_fcs(dir, "a.fcs")
  src <- data.frame(file_name = "a.fcs", md5 = unname(as.character(tools::md5sum(p))))
  cells <- data.frame(file_name = "a.fcs", sample_id = "S1", event_number = 1:3, code = 1)
  expect_no_error(write_annotated_fcs(cells, dir, out, source_files = src))
  make_fcs(dir, "a.fcs", seed = 99)   # rewrite with different content
  err <- expect_error(write_annotated_fcs(cells, dir, out, source_files = src),
                      class = "marmot_fcs_changed")
  expect_match(conditionMessage(err), "a.fcs", fixed = TRUE)
})

test_that("event numbers outside the file stop the export", {
  dir <- withr::local_tempdir(); out <- withr::local_tempdir()
  make_fcs(dir, "a.fcs", n = 60)
  mk <- function(ev) data.frame(file_name = "a.fcs", sample_id = "S1", event_number = ev, code = 1)
  expect_error(write_annotated_fcs(mk(c(1L, 61L)), dir, out), "outside")
  expect_error(write_annotated_fcs(mk(c(0L, 5L)), dir, out), "outside")
  expect_error(write_annotated_fcs(mk(c(NA, 5L)), dir, out), "no event number")
})

test_that("odd sample names give safe file names", {
  dir <- withr::local_tempdir(); out <- withr::local_tempdir()
  make_fcs(dir, "a.fcs")
  cells <- data.frame(file_name = "a.fcs", sample_id = "CD3/α d7", event_number = 1:3, code = 1)
  written <- write_annotated_fcs(cells, dir, out)
  expect_true(file.exists(written))
  expect_equal(dirname(written), out)
  expect_false(grepl("[/\\\\]", basename(written)))
  expect_match(basename(written), "_modified.fcs$")
})
