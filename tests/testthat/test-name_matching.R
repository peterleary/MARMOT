test_that("names that differ only in capitals, spaces, hyphens or underscores share a key", {
  expect_identical(length(unique(name_key(c("IL7R", "Il7R", "IL-7R", "IL 7R", "il_7r")))), 1L)
  expect_false(name_key("CD4") == name_key("CD45"))
  expect_identical(name_key(NA), NA_character_)
})

test_that("an exact match is used as it is and not reported", {
  m <- match_names(c("CD3", "CD8a"), c("CD3", "CD8a", "CD8-A"))
  expect_identical(m$matched, c("CD3", "CD8a"))
  expect_identical(nrow(m$notes), 0L)
})

test_that("a single forgiving match is used and reported", {
  m <- match_names("IL7R", c("CD3", "Il7R"))
  expect_identical(m$matched, "Il7R")
  expect_identical(m$notes$how, "forgiving")
  expect_identical(m$notes$used, "Il7R")
})

test_that("among several forgiving matches, the capitals-only one is chosen and reported", {
  m <- match_names("cd8a", c("CD8-A", "CD8A"))
  expect_identical(m$matched, "CD8A")
  expect_identical(m$notes$how, "chosen")
  expect_match(m$notes$candidates, "CD8-A, CD8A")
})

test_that("without a capitals-only match, the first forgiving match is chosen", {
  m <- match_names("CD8 a", c("CD8-A", "CD8_a"))
  expect_identical(m$matched, "CD8-A")
  expect_identical(m$notes$how, "chosen")
})

test_that("a name with no match is reported as missing", {
  m <- match_names(c("CD3", "CD999"), c("CD3", "CD4"))
  expect_identical(m$matched, c("CD3", NA))
  expect_identical(m$notes$how, "missing")
})

test_that("the notes are turned into plain sentences", {
  m <- match_names(c("IL7R", "CD999"), c("Il7R", "CD3"))
  s <- describe_name_matches(m$notes, "marker", "the FCS files")
  expect_length(s, 2)
  expect_match(s[1], "'IL7R' was matched to 'Il7R'", fixed = TRUE)
  expect_match(s[2], "'CD999' was not found", fixed = TRUE)
  expect_identical(describe_name_matches(m$notes[0, ], "marker", "x"), character())
})
