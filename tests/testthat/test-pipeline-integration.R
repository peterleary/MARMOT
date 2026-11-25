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

  # The first run's Overview warnings are shown again
  saved <- read_overview_warnings(r_files)
  expect_false(is.null(saved))
  html <- paste(readLines(file.path(reload_dir, "MARMOT_Pipeline_Reload.html"), warn = FALSE), collapse = "\n")
  expect_match(html, "are from the original run", fixed = TRUE)
  for (title in sub(".*title='([^']+)'.*", "\\1", saved)) expect_match(html, title, fixed = TRUE)
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

  # The gate must actually gate: some cells pass, and they are the ones high
  # in the gate's markers (once, every cell came out "Negative")
  h5ad <- list.files(test_dir, pattern = "marmot_results\\.h5ad$", recursive = TRUE, full.names = TRUE)
  e <- new.env()
  invisible(capture.output(load_h5ad_to_env(h5ad, envir = e)))
  gated <- as.character(e$sce$Gated_Cells)
  expect_true(any(gated == "Pop"))
  expect_true(all(gated %in% c("Pop", "Negative")))
  ex <- SummarizedExperiment::assay(e$sce, "exprsTransformed")
  expect_gt(median(ex["Marker_1", gated == "Pop"]), median(ex["Marker_1", gated != "Pop"]))
})

test_that("with scGate off the report has no stray All tab", {
  skip_pipeline_deps()

  result <- run_pipeline_test(test_name = "NoScGate", n_cells = 300, n_markers = 6)
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)
  html <- file.path(result$results_path, "MARMOT_Pipeline_NoScGate.html")
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_false(grepl(">\\s*All\\s*</a>", text))
})

test_that("a condition missing from Conditions Order stops the run at the start", {
  skip_pipeline_deps()

  test_dir <- make_test_pipeline_data(n_cells = 200, n_markers = 6)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")
  file_data <- openxlsx::read.xlsx(meta_path, sheet = "File Data")
  file_data$condition[1] <- "Placebo"          # not in Conditions Order at all
  wb <- openxlsx::loadWorkbook(meta_path)
  openxlsx::writeData(wb, "File Data", file_data)
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)

  expect_error(suppressMessages(marmot(metadata = meta_path, name = "BadCond", render = TRUE)))
  # It stopped while reading the metadata, before any results were written
  expect_length(list.files(test_dir, pattern = "marmot_results\\.h5ad$", recursive = TRUE), 0)
})

test_that("files that spell markers and order channels differently are lined up, and the report says so", {
  skip_pipeline_deps()

  test_dir <- make_test_pipeline_data(n_cells = 200, n_markers = 6)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  # One file exported "another week": a marker spelt differently, channels in another order
  f <- file.path(test_dir, "Sample_003.fcs")
  ff <- flowCore::read.FCS(f, transformation = FALSE, truncate_max_range = FALSE)
  ff <- ff[, rev(flowCore::colnames(ff))]
  d <- flowCore::markernames(ff); d[d == "Marker_1"] <- "MARKER-1"; flowCore::markernames(ff) <- d
  suppressWarnings(flowCore::write.FCS(ff, f))

  marmot(metadata = file.path(test_dir, "MARMOT_metadata.xlsx"), name = "Lineup", render = TRUE)
  html <- list.files(test_dir, pattern = "MARMOT_Pipeline_Lineup\\.html$", recursive = TRUE, full.names = TRUE)
  expect_length(html, 1)
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "Names lined up", fixed = TRUE)
  expect_match(text, "MARKER-1.{1,12} in 1 file\\(s\\) was taken as .{1,12}Marker_1")
})

test_that("a marker without a cofactor stops the run with a clear message", {
  skip_pipeline_deps()

  test_dir <- make_test_pipeline_data(n_cells = 200, n_markers = 6)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")
  cof <- openxlsx::read.xlsx(meta_path, sheet = "Study Data", check.names = FALSE, sep.names = " ")$`Cofactors for markers to use`
  cof[3] <- NA
  edit_test_metadata(meta_path, study = list(`Cofactors for markers to use` = cof))
  expect_error(suppressMessages(marmot(metadata = meta_path, name = "NoCof", render = TRUE)))
  expect_length(list.files(test_dir, pattern = "marmot_results\\.h5ad$", recursive = TRUE), 0)
})

test_that("a condition spelt differently from Conditions Order is lined up, and the report says so", {
  skip_pipeline_deps()

  test_dir <- make_test_pipeline_data(n_cells = 200, n_markers = 6)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")
  file_data <- openxlsx::read.xlsx(meta_path, sheet = "File Data")
  file_data$condition[file_data$condition == "Treatment"] <- "treat-ment"
  wb <- openxlsx::loadWorkbook(meta_path)
  openxlsx::writeData(wb, "File Data", file_data)
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)

  marmot(metadata = meta_path, name = "CondCase", render = TRUE)
  html <- list.files(test_dir, pattern = "MARMOT_Pipeline_CondCase\\.html$", recursive = TRUE, full.names = TRUE)
  expect_length(html, 1)
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "Names lined up", fixed = TRUE)
  expect_match(text, "treat-ment", fixed = TRUE)
})

test_that("batch correction runs, mixes the batches and keeps the report honest", {
  skip_pipeline_deps()
  skip_if_not_installed("harmony")

  test_dir <- make_realistic_pipeline_data(n_cells = 800)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")
  sheets <- openxlsx::getSheetNames(meta_path)
  dat <- setNames(lapply(sheets, function(s) openxlsx::read.xlsx(meta_path, sheet = s, check.names = FALSE, sep.names = " ")), sheets)
  # Replicate 1 in one batch, replicates 2 and 3 in another: every batch holds every condition
  dat[["File Data"]]$Batch <- ifelse(grepl("rep1", dat[["File Data"]]$file_name), "day1", "day2")
  ps <- dat[["Pipeline Settings"]]
  ps$Setting[ps$Variable == "clusteringMethodToUse"] <- "MfastPG"
  ps <- rbind(ps, data.frame(Variable = "batchColumn", Setting = "batch"))   # spelt loosely on purpose
  dat[["Pipeline Settings"]] <- ps
  wb <- openxlsx::createWorkbook()
  for (s in sheets) { openxlsx::addWorksheet(wb, s); openxlsx::writeData(wb, s, dat[[s]]) }
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)

  # A real batch effect: five markers read brighter on day 2
  for (f in dat[["File Data"]]$file_name[dat[["File Data"]]$Batch == "day2"]) {
    path <- file.path(test_dir, f)
    ff <- flowCore::read.FCS(path, transformation = FALSE, truncate_max_range = FALSE)
    m <- flowCore::exprs(ff)
    shifted <- c("FJComp-BUV496-A", "FJComp-BUV661-A", "FJComp-BV421-A", "FJComp-BV510-A", "FJComp-PE-A")
    m[, shifted] <- m[, shifted] * 3 + 3000
    flowCore::exprs(ff) <- m
    suppressWarnings(flowCore::write.FCS(ff, path))
  }

  marmot(metadata = meta_path, name = "Batch", render = TRUE)
  html <- list.files(test_dir, pattern = "MARMOT_Pipeline_Batch\\.html$", recursive = TRUE, full.names = TRUE)
  expect_length(html, 1)
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "Batch correction", fixed = TRUE)
  expect_match(text, "Marker values are not corrected", fixed = TRUE)
  mix <- as.numeric(regmatches(text, regexec("Batch mixing:</strong> ([0-9.]+) before correction, ([0-9.]+) after", text))[[1]][2:3])
  expect_true(all(is.finite(mix)))
  expect_gt(mix[2], mix[1] + 0.2)   # correction mixes the batches

  # The batch term reached the DA model, and the corrected positions were saved
  sce <- MARMOT:::reconstruct_sce_from_h5ad(list.files(test_dir, "marmot_results\\.h5ad$", recursive = TRUE, full.names = TRUE))
  expect_true(all(c("PCA_uncorrected", "PCA_harmony", "UMAP_uncorrected") %in% SingleCellExperiment::reducedDimNames(sce)))
  settings <- jsonlite::fromJSON(list.files(test_dir, "pipeline_settings\\.json$", recursive = TRUE, full.names = TRUE))
  expect_identical(settings$batchColumn, "Batch")   # the sheet said "batch"; File Data's spelling is kept
})

test_that("batch correction with FlowSOM stops at the start with a clear message", {
  skip_pipeline_deps()

  test_dir <- make_realistic_pipeline_data(n_cells = 200)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")
  sheets <- openxlsx::getSheetNames(meta_path)
  dat <- setNames(lapply(sheets, function(s) openxlsx::read.xlsx(meta_path, sheet = s, check.names = FALSE, sep.names = " ")), sheets)
  dat[["File Data"]]$Batch <- ifelse(grepl("rep1", dat[["File Data"]]$file_name), "day1", "day2")
  dat[["Pipeline Settings"]] <- rbind(dat[["Pipeline Settings"]], data.frame(Variable = "batchColumn", Setting = "Batch"))
  wb <- openxlsx::createWorkbook()
  for (s in sheets) { openxlsx::addWorksheet(wb, s); openxlsx::writeData(wb, s, dat[[s]]) }
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)
  expect_error(suppressMessages(marmot(metadata = meta_path, name = "BatchSOM", render = TRUE)))
  expect_length(list.files(test_dir, pattern = "marmot_results\\.h5ad$", recursive = TRUE), 0)
})

test_that("label, then regenerate: populations merge, a cluster drops, and DA/DS run on a reload", {
  skip_pipeline_deps()

  result <- run_pipeline_test(test_name = "First", n_cells = 300, n_markers = 6)
  on.exit(unlink(result$test_dir, recursive = TRUE), add = TRUE)
  labels_path <- file.path(result$results_path, "MARMOT_Cluster_Labels.xlsx")
  expect_true(file.exists(labels_path))

  tab <- openxlsx::read.xlsx(labels_path, sheet = "Cluster Labels", check.names = FALSE, sep.names = " ")
  expect_gt(nrow(tab), 2)
  tab$Label <- NA_character_
  tab$Label[1:2] <- c("Pop A", "pop-a")         # merge, spelt loosely
  tab$Label[nrow(tab)] <- "drop"
  wb <- openxlsx::loadWorkbook(labels_path)
  openxlsx::writeData(wb, "Cluster Labels", tab)
  openxlsx::saveWorkbook(wb, labels_path, overwrite = TRUE)

  meta_path <- file.path(result$test_dir, "MARMOT_metadata.xlsx")
  edit_test_metadata(meta_path, settings = list(RDataFolder = result$results_path))
  Sys.sleep(1.1)
  marmot(metadata = meta_path, name = "Labelled", render = TRUE)

  html <- list.files(result$test_dir, pattern = "MARMOT_Pipeline_Labelled\\.html$", recursive = TRUE, full.names = TRUE)
  expect_length(html, 1)
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "Labelled populations", fixed = TRUE)
  expect_match(text, "Clusters dropped", fixed = TRUE)
  expect_match(text, "were merged into", fixed = TRUE)
  xl <- file.path(result$results_path, "Excel_Files", "Labelled_populations_DA_DS.xlsx")
  expect_true(file.exists(xl))
  sheets <- openxlsx::getSheetNames(xl)
  expect_identical(sheets[1:2], c("Populations", "Per sample"))
  da <- openxlsx::read.xlsx(xl, sheet = grep("^DA ", sheets)[1])
  expect_true("Pop A" %in% da$cluster_id)

  # Each cluster's population, and each sample's share, out of the cells left
  pops <- openxlsx::read.xlsx(xl, sheet = "Populations")
  expect_identical(pops$Population[1:2], c("Pop A", "Pop A"))
  expect_identical(pops$Population[nrow(pops)], "dropped")
  per <- openxlsx::read.xlsx(xl, sheet = "Per sample")
  expect_false("dropped" %in% per$Population)
  expect_equal(as.numeric(tapply(per$Percent, per$Sample, sum)), rep(100, length(unique(per$Sample))), tolerance = 0.1)

  # The record the Explorer reads matches the report
  rec <- read_labelled_populations(file.path(result$results_path, "R_files"))
  expect_identical(rec$population, pops$Population)
  expect_identical(rec$cells, as.integer(pops$Cells))
})

test_that("Greek letters in a condition and in display names render, PDFs included", {
  skip_pipeline_deps()

  gamma <- intToUtf8(0x3b3); alpha <- intToUtf8(0x3b1)   # the file stays ASCII
  treated <- paste0("IFN-", gamma, " treated")
  test_dir <- make_test_pipeline_data(n_cells = 300, n_markers = 6, params = list(gimmePDFs = "TRUE"))
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  meta_path <- file.path(test_dir, "MARMOT_metadata.xlsx")

  sheets <- openxlsx::getSheetNames(meta_path)
  data <- lapply(sheets, function(s) openxlsx::read.xlsx(meta_path, sheet = s, check.names = FALSE, sep.names = " "))
  names(data) <- sheets
  data[["File Data"]]$condition[data[["File Data"]]$condition == "Treatment"] <- treated
  sd <- data[["Study Data"]]
  sd$`Conditions Order`[sd$`Conditions Order` %in% "Treatment"] <- treated
  sd$`Conditions To Test`[1] <- paste(treated, "vs Control")
  sd$`Display names for markers` <- NA_character_
  sd$`Display names for markers`[1:2] <- c(paste0("IFN-", gamma), paste0("CD8", alpha))
  data[["Study Data"]] <- sd
  wb <- openxlsx::createWorkbook()
  for (s in sheets) { openxlsx::addWorksheet(wb, s); openxlsx::writeData(wb, s, data[[s]]) }
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)

  marmot(metadata = meta_path, name = "Greek", render = TRUE)
  html <- list.files(test_dir, pattern = "MARMOT_Pipeline_Greek\\.html$", recursive = TRUE, full.names = TRUE)
  expect_length(html, 1)

  # The PDFs draw the letters with the Symbol font, not as dots
  pdfs <- list.files(dirname(html), pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
  expect_gt(length(pdfs), 5)
  uses_symbol <- vapply(pdfs, function(f) any(grepl("/BaseFont /Symbol", readLines(f, warn = FALSE), useBytes = TRUE)), TRUE)
  expect_gt(sum(uses_symbol), 3)
})
