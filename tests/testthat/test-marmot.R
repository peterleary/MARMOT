# Tests for R/marmot.R — Pipeline entry point

test_that("marmot with render=FALSE generates qmd file", {
  # Create a minimal mock metadata xlsx with Pipeline Settings sheet
  tmp <- withr::local_tempdir()
  tmp_metadata <- file.path(tmp, "test_metadata.xlsx")

  settings <- data.frame(
    Variable = c("clusteringMethodToUse", "markersToClusterBy", "kValuesIWant",
                 "knn", "dimRedMethodToUse", "markersToDimRedBy",
                 "runQC", "useQC", "gimmePDFs",
                 "quantileNormaliseAll", "runInParallel", "nCores", "ramPerCore",
                 "themeToUse", "viridisColour"),
    Setting = c("FlowSOM", "Marker1,Marker2", "20",
                "30", "PaCMAP", "Marker1,Marker2",
                "PeacoQC", "TRUE", "TRUE",
                "TRUE", "FALSE", "1", "4",
                "theme_classic", "viridis"),
    stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", settings)
  openxlsx::saveWorkbook(wb, tmp_metadata, overwrite = TRUE)

  expect_message(
    marmot(metadata = tmp_metadata, name = "TestRun", render = FALSE),
    "Generated a modified copy"
  )

  # QMD now lives inside Results_Files_* subdirectory
  results_dirs <- list.dirs(tmp, recursive = FALSE, full.names = TRUE)
  results_dir <- grep("Results_Files_", results_dirs, value = TRUE)
  expect_length(results_dir, 1)

  output_qmd <- file.path(results_dir, "MARMOT_Pipeline_TestRun.qmd")
  expect_true(file.exists(output_qmd))

  content <- readLines(output_qmd)
  expect_true(any(grepl("TestRun", content)))
})

test_that("marmot errors on missing Pipeline Settings tab", {
  tmp <- withr::local_tempdir()
  tmp_metadata <- file.path(tmp, "bad_metadata.xlsx")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::writeData(wb, "Sheet 1", data.frame(x = 1))
  openxlsx::saveWorkbook(wb, tmp_metadata, overwrite = TRUE)

  expect_error(
    marmot(metadata = tmp_metadata),
    "Pipeline Settings"
  )
})

test_that("marmot errors when metadata is NULL", {
  expect_error(
    marmot(metadata = NULL),
    "metadata argument empty"
  )
})

# ── Nullable parameter handling ─────────────────────────────────────────────
# downsampleTo / RDataFolder / excludeTheseSamples are blank-in-Excel → NULL
# in QMD. They must NOT receive a substitution that emits an empty/NA value.

test_that("marmot: blank nullable params leave QMD defaults intact (NULL)", {
  tmp <- withr::local_tempdir()
  tmp_metadata <- file.path(tmp, "nullable_metadata.xlsx")

  settings <- data.frame(
    Variable = c("clusteringMethodToUse", "markersToClusterBy", "kValuesIWant",
                 "knn", "dimRedMethodToUse", "markersToDimRedBy",
                 "runQC", "useQC", "gimmePDFs",
                 "quantileNormaliseAll", "runInParallel", "nCores", "ramPerCore",
                 "themeToUse", "viridisColour",
                 "downsampleTo", "RDataFolder", "excludeTheseSamples"),
    Setting = c("FlowSOM", "all", "10",
                "10", "UMAP", "all",
                "None", "FALSE", "FALSE",
                "FALSE", "FALSE", "1", "4",
                "prism", "viridis",
                NA, NA, NA),
    stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", settings)
  openxlsx::saveWorkbook(wb, tmp_metadata, overwrite = TRUE)

  marmot(metadata = tmp_metadata, name = "Nullable", render = FALSE)

  results_dirs <- list.dirs(tmp, recursive = FALSE, full.names = TRUE)
  results_dir  <- grep("Results_Files_", results_dirs, value = TRUE)
  output_qmd   <- file.path(results_dir, "MARMOT_Pipeline_Nullable.qmd")
  content      <- readLines(output_qmd)

  # Each nullable var keeps its NULL default — no substitution corrupted it.
  expect_true(any(grepl("^downsampleTo\\s*<-\\s*NULL", content)),
              info = "downsampleTo should remain NULL")
  expect_true(any(grepl("^RDataFolder\\s*<-\\s*NULL", content)),
              info = "RDataFolder should remain NULL")
  expect_true(any(grepl("^excludeTheseSamples\\s*<-\\s*NULL", content)),
              info = "excludeTheseSamples should remain NULL")

  # Negative: must NOT have been substituted to a stray value (empty string,
  # NA, character literal). This guards against the bug where length(NULL)==0
  # falls through both branches and `replacement` carries over from a prior
  # iteration.
  for (var in c("downsampleTo", "RDataFolder", "excludeTheseSamples")) {
    line_idx <- grep(paste0("^", var, "\\s*<-"), content)
    expect_length(line_idx, 1L)
    line <- content[line_idx]
    expect_false(grepl("<-\\s*NA\\b", line),  info = paste(var, "must not be NA"))
    expect_false(grepl("<-\\s*''",   line),   info = paste(var, "must not be empty string"))
    expect_false(grepl("<-\\s*\"\"", line),   info = paste(var, "must not be empty string"))
  }
})

test_that("marmot: concrete nullable params get substituted correctly", {
  tmp <- withr::local_tempdir()
  tmp_metadata <- file.path(tmp, "concrete_metadata.xlsx")

  settings <- data.frame(
    Variable = c("clusteringMethodToUse", "markersToClusterBy", "kValuesIWant",
                 "knn", "dimRedMethodToUse", "markersToDimRedBy",
                 "runQC", "useQC", "gimmePDFs",
                 "quantileNormaliseAll", "runInParallel", "nCores", "ramPerCore",
                 "themeToUse", "viridisColour",
                 "downsampleTo", "excludeTheseSamples"),
    Setting = c("FlowSOM", "all", "10",
                "10", "UMAP", "all",
                "None", "FALSE", "FALSE",
                "FALSE", "FALSE", "1", "4",
                "prism", "viridis",
                "1500", "Sample_001"),
    stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", settings)
  openxlsx::saveWorkbook(wb, tmp_metadata, overwrite = TRUE)

  marmot(metadata = tmp_metadata, name = "Concrete", render = FALSE)

  results_dirs <- list.dirs(tmp, recursive = FALSE, full.names = TRUE)
  results_dir  <- grep("Results_Files_", results_dirs, value = TRUE)
  output_qmd   <- file.path(results_dir, "MARMOT_Pipeline_Concrete.qmd")
  content      <- readLines(output_qmd)

  expect_true(any(grepl("^downsampleTo\\s*<-\\s*1500\\b", content)))
  expect_true(any(grepl("^excludeTheseSamples\\s*<-\\s*'Sample_001'", content)))
})

test_that(".find_quarto_pandoc finds the pandoc that ships with Quarto", {
  find_quarto_pandoc <- getFromNamespace(".find_quarto_pandoc", "MARMOT")
  d <- withr::local_tempdir()
  bin <- file.path(d, "bin"); dir.create(bin)
  quarto <- file.path(bin, "quarto"); file.create(quarto)

  # No tools folder, or no quarto at all: nothing found
  expect_equal(find_quarto_pandoc(quarto), "")
  expect_equal(find_quarto_pandoc(""), "")

  # Older layout: bin/tools/pandoc
  dir.create(file.path(bin, "tools"))
  file.create(file.path(bin, "tools", "pandoc"))
  expect_equal(normalizePath(find_quarto_pandoc(quarto)), normalizePath(file.path(bin, "tools")))

  # Newer layout: one folder per processor type; this machine's is preferred
  unlink(file.path(bin, "tools", "pandoc"))
  machine <- Sys.info()[["machine"]]
  if (machine == "arm64") machine <- "aarch64"
  for (m in unique(c("aaa_other", machine))) {
    dir.create(file.path(bin, "tools", m))
    file.create(file.path(bin, "tools", m, "pandoc"))
  }
  expect_equal(basename(find_quarto_pandoc(quarto)), machine)
})

test_that(".find_utf8_ctype finds a UTF-8 character type and leaves the session alone", {
  find_utf8_ctype <- getFromNamespace(".find_utf8_ctype", "MARMOT")
  before <- Sys.getlocale("LC_CTYPE")
  found <- find_utf8_ctype()
  expect_identical(Sys.getlocale("LC_CTYPE"), before)
  expect_equal(find_utf8_ctype(candidates = "no_SUCH.locale"), "")
  skip_if(!nzchar(found), "this machine offers no UTF-8 character type")
  withr::with_locale(c(LC_CTYPE = found), expect_true(l10n_info()[["UTF-8"]]))
})

# Minimal metadata workbook for the settings tests below
make_settings_workbook <- function(dir, settings, info = "note") {
  tmpl <- system.file("pipeline", "MARMOT_Metadata.xlsx", package = "MARMOT")
  wb <- openxlsx::loadWorkbook(tmpl)
  ps <- openxlsx::read.xlsx(wb, sheet = "Pipeline Settings")
  for (v in names(settings)) {
    row <- match(v, ps$Variable) + 1L
    openxlsx::writeData(wb, "Pipeline Settings", settings[[v]], startCol = 2, startRow = row, colNames = FALSE)
    if (is.na(info)) openxlsx::deleteData(wb, "Pipeline Settings", cols = 3, rows = row, gridExpand = TRUE)
  }
  path <- file.path(dir, "meta.xlsx")
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}
run_copy <- function(dir) {
  qmd <- list.files(dir, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)
  readLines(qmd[1])
}

test_that("marmot: a blank Info cell does not drop the setting", {
  d <- withr::local_tempdir()
  md <- make_settings_workbook(d, list(knn = "35", kValuesIWant = "35 50"), info = NA)
  suppressMessages(marmot(metadata = md, name = "InfoTest", render = FALSE))
  lines <- run_copy(d)
  expect_true(any(grepl("^knn <- 35$", lines)))
  expect_true(any(grepl("^kValuesIWant <- c\\(35,50\\)$", lines)))
})

test_that("marmot: backslashes and quotes in a value survive into the run copy", {
  d <- withr::local_tempdir()
  md <- make_settings_workbook(d, list(RDataFolder = "C:\\Users\\me\\O'Brien\\R_files"))
  suppressMessages(marmot(metadata = md, name = "PathTest", render = FALSE))
  line <- grep("^RDataFolder <- ", run_copy(d), value = TRUE)
  expect_length(line, 1)
  value <- eval(parse(text = sub("^RDataFolder <- ", "", line)))
  expect_identical(value, "C:\\Users\\me\\O'Brien\\R_files")
})

test_that("marmot: k values may be separated by commas, spaces or both", {
  d <- withr::local_tempdir()
  md <- make_settings_workbook(d, list(kValuesIWant = "20, 40,60", knn = "20"))
  suppressMessages(marmot(metadata = md, name = "KTest", render = FALSE))
  expect_true(any(grepl("^kValuesIWant <- c\\(20,40,60\\)$", run_copy(d))))
})

test_that("marmot: a blank downsampleTo means every cell, not the default cap", {
  d <- withr::local_tempdir()
  md <- make_settings_workbook(d, list(knn = "20"))
  wb <- openxlsx::loadWorkbook(md)
  ps <- openxlsx::read.xlsx(wb, sheet = "Pipeline Settings")
  openxlsx::deleteData(wb, "Pipeline Settings", cols = 2, rows = match("downsampleTo", ps$Variable) + 1L, gridExpand = TRUE)
  openxlsx::saveWorkbook(wb, md, overwrite = TRUE)
  suppressMessages(marmot(metadata = md, name = "BlankDS", render = FALSE))
  expect_true(any(grepl("^downsampleTo <- NULL$", run_copy(d))))
})

test_that("marmot: retired settings are set aside with a message, unknown ones warned about", {
  d <- withr::local_tempdir()
  sheet <- data.frame(
    Variable = c("knn", "kValuesIWant", "runInParallel", "themeToUse", "condaDir", "knnn"),
    Setting  = c("40", "20 40", "TRUE", "bw", "~/mamba", "30"),
    Info     = "x", stringsAsFactors = FALSE)
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", sheet)
  md <- file.path(d, "meta.xlsx"); openxlsx::saveWorkbook(wb, md)
  expect_message(
    expect_warning(marmot(metadata = md, name = "Old", render = FALSE), "knnn"),
    "no longer used.*runInParallel, themeToUse, condaDir")
  lines <- run_copy(d)
  expect_true(any(grepl("^knn <- 40$", lines)))
  expect_false(any(grepl("themeToUse|runInParallel|condaDir|knnn", lines)))
  # Settings missing from the sheet keep MARMOT's defaults
  expect_true(any(grepl("^downsampleTo <- 10000", lines)))
  expect_true(any(grepl("^viridisColour <- \"lapaz\"", lines)))
})

test_that("marmot: a setting left blank that needs a value stops with its name", {
  d <- withr::local_tempdir()
  sheet <- data.frame(Variable = c("knn", "kValuesIWant"), Setting = c(NA, "20 40"), Info = "x")
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", sheet)
  md <- file.path(d, "meta.xlsx"); openxlsx::saveWorkbook(wb, md)
  expect_error(marmot(metadata = md, name = "Blank", render = FALSE), "knn is blank")
})

test_that("marmot: excludeTheseSamples takes a comma-separated list", {
  d <- withr::local_tempdir()
  sheet <- data.frame(Variable = c("knn", "excludeTheseSamples"), Setting = c("20", "S001, S 02,S003"), Info = "x")
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", sheet)
  md <- file.path(d, "meta.xlsx"); openxlsx::saveWorkbook(wb, md)
  suppressMessages(marmot(metadata = md, name = "Excl", render = FALSE))
  expect_true(any(grepl("^excludeTheseSamples <- c\\('S001','S 02','S003'\\)$", run_copy(d))))
})
