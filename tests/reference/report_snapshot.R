#!/usr/bin/env Rscript
# report_snapshot.R -- summarise what a MARMOT report shows as a small JSON
# file, so that two reports can be compared with report_compare.R.
#
# Usage:
#   Rscript tests/reference/report_snapshot.R <report.html> <files_dir> <out.json> [--mask <path>]...
#
# <files_dir> is searched for the PDF, Excel and CSV files written beside the
# report. Each --mask path is replaced by "<DIR>" wherever the report prints it,
# so that two renders in different folders compare as equal.
#
# The snapshot holds:
#   text   every line of text the reader sees, in order, with a marker line
#          for each figure and each interactive table (the marker carries a
#          digest of the picture or of the table's contents). Code listings
#          are left out: they change whenever the code does.
#   files  a digest of every PDF (ignoring its creation date), every sheet of
#          every Excel file, and every CSV file.

args <- commandArgs(trailingOnly = TRUE)
masks <- character()
while ("--mask" %in% args) {
  i <- match("--mask", args)
  masks <- c(masks, args[i + 1L])
  args <- args[-c(i, i + 1L)]
}
if (length(args) != 3L || !file.exists(args[[1]]) || !dir.exists(args[[2]])) {
  message("usage: Rscript tests/reference/report_snapshot.R <report.html> <files_dir> <out.json> [--mask <path>]...")
  quit(save = "no", status = 2L)
}
for (p in c("xml2", "jsonlite", "digest", "openxlsx")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("report_snapshot.R: ", p, " is required")
    quit(save = "no", status = 2L)
  }
}
html <- args[[1]]; files_dir <- args[[2]]; out <- args[[3]]

md5 <- function(x) digest::digest(x, algo = "md5", serialize = FALSE)

# ---- the report --------------------------------------------------------------

doc <- xml2::read_html(html, options = c("RECOVER", "NOERROR", "NOBLANKS", "HUGE"))

# Figures: leave a marker where each embedded picture was
figures <- xml2::xml_find_all(doc, "//img[starts-with(@src, 'data:')]")
for (node in figures) {
  xml2::xml_add_sibling(node, "p", paste0("\n[[figure ", md5(xml2::xml_attr(node, "src")), "]]\n"))
}
n_figures <- length(figures)

# Interactive tables: leave a marker where each one is drawn
widgets <- xml2::xml_find_all(doc, "//script[@type='application/json' and @data-for]")
n_tables <- 0L
for (node in widgets) {
  id <- xml2::xml_attr(node, "data-for")
  content <- gsub("htmlwidget-[0-9a-f]+", "htmlwidget", xml2::xml_text(node))
  target <- xml2::xml_find_first(doc, sprintf("//*[@id='%s']", id))
  if (!inherits(target, "xml_missing")) {
    xml2::xml_add_sibling(target, "p", paste0("\n[[table ", md5(content), "]]\n"))
    n_tables <- n_tables + 1L
  }
}

# Drop what the reader does not see, and the code listings
xml2::xml_remove(xml2::xml_find_all(doc, paste(
  "//script", "//style", "//head", "//noscript",
  "//*[contains(concat(' ', normalize-space(@class), ' '), ' sourceCode ')]",
  sep = " | "
)))

text <- xml2::xml_text(xml2::xml_find_first(doc, "//body"))
text <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
text <- text[nzchar(text)]
for (m in masks[order(-nchar(masks))]) text <- gsub(m, "<DIR>", text, fixed = TRUE)
text <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}[_ T][0-9]{2}[.:][0-9]{2}[.:][0-9]{2}", "<TIME>", text)

# ---- the files beside it -----------------------------------------------------

pdf_digest <- function(path) {
  bytes <- readBin(path, "raw", n = file.size(path))
  # The creation and modification dates sit in the clear; blank them out
  for (key in c("/CreationDate", "/ModDate")) {
    for (at in grepRaw(key, bytes, fixed = TRUE, all = TRUE)) {
      end <- min(length(bytes), at + nchar(key) + 40L)
      close <- grepRaw(")", bytes[at:end], fixed = TRUE)
      if (length(close)) bytes[at:(at + close[1] - 1L)] <- as.raw(32L)
    }
  }
  md5(bytes)
}

excel_digest <- function(path) {
  sheets <- openxlsx::getSheetNames(path)
  stats::setNames(lapply(sheets, function(s) {
    df <- tryCatch(openxlsx::read.xlsx(path, sheet = s), error = function(e) NULL)
    if (is.null(df)) return("empty")
    # Numbers to 10 significant figures: beyond that is arithmetic noise
    df[] <- lapply(df, function(col) if (is.numeric(col)) signif(col, 10) else col)
    digest::digest(df, algo = "md5")
  }), sheets)
}

paths <- sort(list.files(files_dir, pattern = "\\.(pdf|xlsx|csv)$", recursive = TRUE, ignore.case = TRUE))
files <- stats::setNames(lapply(paths, function(rel) {
  full <- file.path(files_dir, rel)
  switch(tolower(tools::file_ext(rel)),
    pdf  = pdf_digest(full),
    xlsx = excel_digest(full),
    md5(readBin(full, "raw", n = file.size(full)))
  )
}), paths)

jsonlite::write_json(
  list(report = basename(html), n_figures = n_figures, n_tables = n_tables, text = text, files = files),
  out, auto_unbox = TRUE, pretty = TRUE
)
cat("snapshot written: ", basename(out), " (", length(text), " lines, ", n_figures, " figures, ",
    n_tables, " tables, ", length(files), " files)\n", sep = "")
