make_fcs <- function(channels, keywords = list()) {
  m <- matrix(runif(10 * length(channels)), 10, length(channels),
              dimnames = list(NULL, channels))
  ff <- flowCore::flowFrame(m)
  for (k in names(keywords)) flowCore::keyword(ff)[[k]] <- keywords[[k]]
  f <- tempfile(fileext = ".fcs")
  flowCore::write.FCS(ff, f)
  f
}

test_that("the shipped table reads and has the expected columns", {
  tab <- qc_ignore_table()
  expect_named(tab, c("instrument", "channel", "reason"))
  expect_false(any(duplicated(tab[c("instrument", "channel")])))
  expect_true("SaturatedChannels" %in% tab$channel[tab$instrument == "FACSDiscover S8"])
})

test_that("channels are found by instrument, ignoring capitals", {
  f <- make_fcs(c("FSC-A", "Sorted", "Size (FSC)", "CD3"),
                list("$CYT" = "facsdiscover s8"))
  res <- qc_ignore_channels(f)
  expect_setequal(res$channel, c("Sorted", "Size (FSC)"))
})

test_that("another instrument's channels are not touched", {
  f <- make_fcs(c("FSC-A", "Sorted", "CD3"), list("$CYT" = "Aurora"))
  expect_equal(nrow(qc_ignore_channels(f)), 0)
})

test_that("a channel stored in a different data type is caught on any instrument", {
  f <- make_fcs(c("FSC-A", "Flag", "CD3"),
                list("$CYT" = "Unknown machine", "$P2DATATYPE" = "I"))
  res <- qc_ignore_channels(f)
  expect_equal(res$channel, "Flag")
  expect_match(res$reason, "cannot read")
})

test_that("channels are counted across files", {
  a <- make_fcs(c("FSC-A", "Sorted", "CD3"), list("$CYT" = "FACSDiscover S8"))
  b <- make_fcs(c("FSC-A", "Sorted", "CD3"), list("$CYT" = "FACSDiscover S8",
                                                 "$P2DATATYPE" = "I"))
  res <- qc_ignore_channels(c(a, b))
  expect_equal(res$channel, "Sorted")
  expect_equal(res$files, 2L)
})

test_that("exact patterns match only their own channel", {
  ch <- c("Size (FSC)", "Center of Mass (Y) (FSC)", "Row")
  pats <- exact_channel_patterns(ch)
  all_ch <- c(ch, "Size (FSC)-A", "Rows", "FSC-A")
  hit <- grep(paste(pats, collapse = "|"), all_ch, value = TRUE)
  expect_setequal(hit, ch)
  expect_length(exact_channel_patterns(character()), 0)
})
