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
