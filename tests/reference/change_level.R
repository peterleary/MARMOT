#!/usr/bin/env Rscript
# change_level.R -- say how much checking the uncommitted changes need.
#
# Usage:
#   Rscript tests/reference/change_level.R [<git ref>]     (default: HEAD)
#
# Compares the working tree with <git ref> and prints one of:
#   LEVEL: none        nothing that can affect a run has changed
#   LEVEL: report      only chunks that run when results are reloaded have
#                      changed, so redrawing a saved run exercises all of it
#   LEVEL: analysis    a chunk that is skipped on reload has changed, so the
#                      pipeline has to be run again
#   LEVEL: everything  code outside the pipeline QMD has changed
# followed by the reasons. Run it from the top of the repository.

ref <- commandArgs(trailingOnly = TRUE)
ref <- if (length(ref)) ref[[1]] else "HEAD"
qmd <- "inst/pipeline/MARMOT_Pipeline.qmd"

git <- function(...) suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE))

changed <- unique(c(git("diff", "--name-only", ref), git("ls-files", "--others", "--exclude-standard")))
changed <- changed[nzchar(changed)]

# Files that cannot change what a run produces
harmless <- grepl("^(tests/|notes/|man/|docs/|images/|tauri-app/|\\.claude/|\\.github/)|\\.md$", changed)
# The report's look (styles, header, toolbar): redrawing a saved run shows all of it
report_look <- changed %in% file.path("inst/pipeline", c("marmot.scss", "marmot_header.html", "marmot-toolbar.html"))
other_code <- setdiff(changed[!harmless & !report_look], qmd)

# Which chunk each line of the QMD belongs to ("" outside a chunk)
chunk_of_lines <- function(lines) {
  name <- ""; out <- character(length(lines))
  for (i in seq_along(lines)) {
    l <- lines[[i]]
    if (grepl("^```\\{r", l)) {
      m <- regmatches(l, regexpr("label *= *\"[^\"]+\"", l))
      name <- if (length(m)) sub("label *= *\"([^\"]+)\"", "\\1", m) else
        sub("^```\\{r[ ,]*([A-Za-z0-9_.-]*).*$", "\\1", l)
      if (!nzchar(name)) name <- "(unnamed chunk)"
      out[i] <- name
    } else if (grepl("^```\\s*$", l) && nzchar(name)) {
      out[i] <- name; name <- ""
    } else {
      out[i] <- name
    }
  }
  out
}

chunks_touched <- character()
if (qmd %in% changed) {
  old <- chunk_of_lines(git("show", paste0(ref, ":", qmd)))
  new <- chunk_of_lines(readLines(qmd, warn = FALSE))
  hunks <- grep("^@@", git("diff", "-U0", ref, "--", qmd), value = TRUE)
  span <- function(txt) {            # "12,3" -> 12:14 ; "12" -> 12 ; "12,0" -> 12
    p <- as.integer(strsplit(txt, ",", fixed = TRUE)[[1]])
    start <- max(p[1], 1L); n <- if (length(p) > 1L) p[2] else 1L
    seq.int(start, start + max(n, 1L) - 1L)
  }
  for (h in hunks) {
    m <- regmatches(h, regexec("^@@ -([0-9,]+) \\+([0-9,]+) @@", h))[[1]]
    a <- span(m[2]); b <- span(m[3])
    chunks_touched <- c(chunks_touched, old[a[a <= length(old)]], new[b[b <= length(new)]])
  }
  chunks_touched <- unique(chunks_touched)
}
in_text <- "" %in% chunks_touched
chunks_touched <- setdiff(chunks_touched, "")

# Chunks that do not run (or do nothing) when results are reloaded
skipped_on_reload <- grepl("^(analysis_|save_results|input_settings|reload_)", chunks_touched)

level <- if (length(other_code)) "everything" else
  if (any(skipped_on_reload)) "analysis" else
  if (length(chunks_touched) || in_text || any(report_look)) "report" else "none"

cat("LEVEL: ", level, "\n", sep = "")
if (length(other_code)) cat("  code outside the QMD: ", paste(other_code, collapse = ", "), "\n", sep = "")
if (any(skipped_on_reload)) cat("  chunks not run on reload: ", paste(chunks_touched[skipped_on_reload], collapse = ", "), "\n", sep = "")
if (any(!skipped_on_reload)) cat("  chunks run on reload: ", paste(chunks_touched[!skipped_on_reload], collapse = ", "), "\n", sep = "")
if (in_text) cat("  report text outside chunks\n")
if (any(report_look)) cat("  the report's look: ", paste(changed[report_look], collapse = ", "), "\n", sep = "")
