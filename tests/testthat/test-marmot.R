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
