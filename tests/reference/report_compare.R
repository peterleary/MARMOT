#!/usr/bin/env Rscript
# report_compare.R -- say exactly what differs between two report snapshots
# written by report_snapshot.R.
#
# Usage:
#   Rscript tests/reference/report_compare.R <before.json> <after.json> [--lines 80]
#
# Exit status: 0 if identical, 1 if different, 2 on a usage or reading error.

args <- commandArgs(trailingOnly = TRUE)
max_lines <- 80L
if ("--lines" %in% args) {
  i <- match("--lines", args)
  max_lines <- as.integer(args[i + 1L])
  args <- args[-c(i, i + 1L)]
}
if (length(args) != 2L || !all(file.exists(args)) || !requireNamespace("jsonlite", quietly = TRUE)) {
  message("usage: Rscript tests/reference/report_compare.R <before.json> <after.json> [--lines 80]")
  quit(save = "no", status = 2L)
}
A <- jsonlite::fromJSON(args[[1]], simplifyVector = TRUE)
B <- jsonlite::fromJSON(args[[2]], simplifyVector = TRUE)

different <- FALSE

# ---- what the reader sees -----------------------------------------------------

ta <- tempfile(); tb <- tempfile()
writeLines(as.character(A$text), ta); writeLines(as.character(B$text), tb)
d <- suppressWarnings(system2("diff", c("-U", "1", shQuote(ta), shQuote(tb)), stdout = TRUE, stderr = TRUE))
d <- d[!grepl("^(---|\\+\\+\\+) ", d)]
changed <- d[grepl("^[-+]", d)]
is_marker <- grepl("^[-+]\\[\\[(figure|table) ", changed)
cat(sprintf("report text      %s\n", if (!any(!is_marker)) "IDENTICAL" else
  sprintf("DIFFERENT (%d lines removed, %d added)", sum(grepl("^-", changed[!is_marker])), sum(grepl("^\\+", changed[!is_marker])))))
for (kind in c("figure", "table")) {
  total <- if (kind == "figure") c(A$n_figures, B$n_figures) else c(A$n_tables, B$n_tables)
  n <- sum(grepl(paste0("^\\+\\[\\[", kind, " "), changed)); gone <- sum(grepl(paste0("^-\\[\\[", kind, " "), changed))
  cat(sprintf("report %-9s %s\n", paste0(kind, "s"),
    if (n == 0L && gone == 0L) sprintf("IDENTICAL (%d)", total[1]) else
      sprintf("DIFFERENT (%d before, %d after; %d no longer match)", total[1], total[2], max(n, gone))))
}
if (length(changed) > 0L) {
  different <- TRUE
  cat("\n-- in the report (- before, + after; unmarked lines are there to show where):\n")
  shown <- utils::head(d, max_lines)
  cat(paste0("   ", substr(shown, 1L, 160L)), sep = "\n")
  if (length(d) > max_lines) cat(sprintf("   ... and %d more lines\n", length(d) - max_lines))
}

# ---- the files beside the report ---------------------------------------------

flat <- function(files) {
  out <- list()
  for (name in names(files)) {
    v <- files[[name]]
    if (is.list(v)) for (s in names(v)) out[[paste0(name, " [sheet ", s, "]")]] <- v[[s]]
    else out[[name]] <- v
  }
  unlist(out)
}
fa <- flat(A$files); fb <- flat(B$files)
gone <- setdiff(names(fa), names(fb)); new <- setdiff(names(fb), names(fa))
both <- intersect(names(fa), names(fb)); altered <- both[fa[both] != fb[both]]
cat(sprintf("\nfiles            %s\n", if (!length(c(gone, new, altered))) sprintf("IDENTICAL (%d)", length(fa)) else
  sprintf("DIFFERENT (%d missing, %d new, %d changed)", length(gone), length(new), length(altered))))
if (length(c(gone, new, altered))) {
  different <- TRUE
  for (f in gone) cat("   missing: ", f, "\n", sep = "")
  for (f in new) cat("   new:     ", f, "\n", sep = "")
  for (f in altered) cat("   changed: ", f, "\n", sep = "")
}

cat("\nRESULT: ", if (different) "DIFFERENT" else "IDENTICAL", "\n", sep = "")
quit(save = "no", status = if (different) 1L else 0L)
