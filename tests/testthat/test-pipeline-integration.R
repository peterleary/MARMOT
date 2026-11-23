# Pipeline Integration Test
# End-to-end: synthetic FCS + metadata → marmot(render=TRUE) → validate outputs

test_that("pipeline integration: FlowSOM + UMAP end-to-end", {
  skip_pipeline_deps()

  result <- run_pipeline_test(
    params = list(),
    test_name = "IntTest",
    n_cells = 500,
    n_markers = 8
  )
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)

  # Common structural validation
  sce <- validate_pipeline_output(result, expected_cells = 2000)

  # Specific SCE dimension checks for baseline
  expect_equal(nrow(sce), 8)         # 8 markers
  expect_equal(ncol(sce), 2000)      # 4 samples × 500 cells

  # UMAP reduction shape
  umap_data <- SingleCellExperiment::reducedDim(sce, "UMAP")
  expect_equal(ncol(umap_data), 2)   # 2 UMAP dims
  expect_equal(nrow(umap_data), 2000)
})

# Helper: change one row of a metadata sheet's Pipeline Settings or Study Data
edit_test_metadata <- function(meta_path, settings = list(), study = list()) {
  sheets <- openxlsx::getSheetNames(meta_path)
  data <- lapply(sheets, function(s) openxlsx::read.xlsx(meta_path, sheet = s, check.names = FALSE, sep.names = " "))
  names(data) <- sheets
  for (nm in names(settings)) data[["Pipeline Settings"]]$Setting[data[["Pipeline Settings"]]$Variable == nm] <- settings[[nm]]
  for (nm in names(study)) data[["Study Data"]][[nm]] <- study[[nm]]
  wb <- openxlsx::createWorkbook()
  for (s in sheets) { openxlsx::addWorksheet(wb, s); openxlsx::writeData(wb, s, data[[s]]) }
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)
}

test_that("a reload redraws the report and keeps the original run log", {
  skip_pipeline_deps()

  result <- run_pipeline_test(test_name = "First", n_cells = 300, n_markers = 6)
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)
  r_files <- file.path(result$results_path, "R_files")
  first_log <- jsonlite::fromJSON(file.path(r_files, "marmot_run_log.json"))
  expect_false(first_log$settings$reload)

  meta_path <- file.path(result$test_dir, "MARMOT_metadata.xlsx")
  edit_test_metadata(meta_path, settings = list(RDataFolder = result$results_path))
  Sys.sleep(1.1)   # results folders are named by the second
  marmot(metadata = meta_path, name = "Reload", render = TRUE)

  reload_dir <- setdiff(list.dirs(result$test_dir, recursive = FALSE), result$results_path)
  reload_dir <- grep("Results_Files_", reload_dir, value = TRUE)
  expect_length(reload_dir, 1)
  expect_true(file.exists(file.path(reload_dir, "MARMOT_Pipeline_Reload.html")))

  # The original run's log is untouched; the reload has its own
  expect_identical(jsonlite::fromJSON(file.path(r_files, "marmot_run_log.json")), first_log)
  reload_log <- jsonlite::fromJSON(file.path(r_files, "marmot_run_log_reload.json"))
  expect_true(reload_log$settings$reload)
  expect_identical(reload_log$render$status, "success")
})

test_that("scGate draws its plots when every cell is shown", {
  skip_pipeline_deps()
  skip_if_not_installed("scGate")
  skip_if_not_installed("UCell")
  skip_if_not_installed("Seurat")

  test_dir <- make_test_pipeline_data(n_cells = 300, n_markers = 6, params = list(runScGate = "TRUE"))
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")
  n_rows <- nrow(openxlsx::read.xlsx(meta_path, sheet = "Study Data"))
  edit_test_metadata(meta_path, study = list(
    `Cells per condition in UMAPs etc.` = rep(NA_character_, n_rows),
    `Marker Pairs` = c("Pop: Marker_1 Marker_2", rep(NA_character_, n_rows - 1))
  ))
  marmot(metadata = meta_path, name = "ScGate", render = TRUE)

  html <- list.files(test_dir, pattern = "MARMOT_Pipeline_ScGate\\.html$", recursive = TRUE, full.names = TRUE)
  expect_length(html, 1)
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "scGate Gating", fixed = TRUE)
})

test_that("with scGate off the report has no stray All tab", {
  skip_pipeline_deps()

  result <- run_pipeline_test(test_name = "NoScGate", n_cells = 300, n_markers = 6)
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)
  html <- file.path(result$results_path, "MARMOT_Pipeline_NoScGate.html")
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_false(grepl(">\\s*All\\s*</a>", text))
})
