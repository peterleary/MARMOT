# The pipeline's settings have defaults in three places: the QMD's settings
# chunk, the metadata template, and the desktop app's "new sheet". They drifted
# apart once (five defaults disagreed). These tests fail if they do so again.

read_qmd_defaults <- function() {
  qmd <- readLines(system.file("pipeline", "MARMOT_Pipeline.qmd", package = "MARMOT"))
  chunk <- qmd[(grep("^```\\{r input_settings\\}", qmd) + 1):length(qmd)]
  chunk <- chunk[seq_len(grep("^```", chunk)[1] - 1)]
  lines <- grep("^[A-Za-z][A-Za-z0-9_.]* <- ", chunk, value = TRUE)
  values <- trimws(sub("\\s+#.*$", "", sub("^[^<]*<- ", "", lines)))
  stats::setNames(values, sub(" <- .*$", "", lines))
}

# One spelling for a default, wherever it came from
normalise <- function(x) {
  x <- gsub("[\"']", "", as.character(x))
  x[is.na(x) | x == "" | x == "NULL"] <- "<none>"
  is_vector <- grepl("^c\\(.*\\)$", x) | grepl("^[0-9]+([ ,]+[0-9]+)+$", x)
  x[is_vector] <- vapply(strsplit(gsub("^c\\(|\\)$", "", x[is_vector]), "[ ,]+"), paste, "", collapse = " ")
  x
}

set_by_marmot <- c("fp", "md_fp", "resultsDir")

test_that("the metadata template and the QMD list the same settings with the same defaults", {
  qmd <- read_qmd_defaults()
  qmd <- qmd[!names(qmd) %in% set_by_marmot]
  tmpl <- openxlsx::read.xlsx(system.file("pipeline", "MARMOT_Metadata.xlsx", package = "MARMOT"),
                              sheet = "Pipeline Settings")
  tmpl <- stats::setNames(tmpl$Setting, tmpl$Variable)
  tmpl <- tmpl[!is.na(names(tmpl))]

  expect_setequal(names(tmpl), names(qmd))
  both <- intersect(names(tmpl), names(qmd))
  expect_equal(normalise(tmpl[both]), normalise(qmd[both]), ignore_attr = TRUE,
               info = paste("settings compared:", paste(both, collapse = ", ")))
})

test_that("the desktop app's new-sheet defaults match the QMD", {
  excel_rs <- testthat::test_path("..", "..", "tauri-app", "src-tauri", "src", "commands", "excel.rs")
  skip_if_not(file.exists(excel_rs), "desktop app source not available (built package)")
  src <- readLines(excel_rs)
  rows <- regmatches(src, regexec('^\\s*\\("([A-Za-z]+)", "([^"]*)", "', src))
  rows <- rows[lengths(rows) == 3]
  app <- stats::setNames(vapply(rows, `[`, "", 3), vapply(rows, `[`, "", 2))

  qmd <- read_qmd_defaults()
  qmd <- qmd[!names(qmd) %in% set_by_marmot]
  expect_setequal(names(app), names(qmd))
  both <- intersect(names(app), names(qmd))
  expect_equal(normalise(app[both]), normalise(qmd[both]), ignore_attr = TRUE,
               info = paste("settings compared:", paste(both, collapse = ", ")))
})

test_that("every option the template offers in a drop-down is one the pipeline accepts", {
  opts <- suppressMessages(openxlsx::read.xlsx(
    system.file("pipeline", "MARMOT_Metadata.xlsx", package = "MARMOT"), sheet = "Options", colNames = FALSE))
  offered <- function(col) { v <- opts[[col]]; v[!is.na(v)] }
  expect_setequal(offered(1), c("FlowSOM", "Rphenograph", "Mphenograph", "MfastPG", "PARC", "Mparc"))
  expect_setequal(offered(3), c("UMAP", "TSNE", "pacmap", "Mpacmap"))
  expect_true("lapaz" %in% unlist(opts))
})
