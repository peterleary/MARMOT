#!/usr/bin/env Rscript
# compare.R -- report exactly what differs between two MARMOT fingerprints
# written by fingerprint.R.
#
# Usage:
#   Rscript tests/reference/compare.R <a.json> <b.json> [--tolerance 0]
#
# Numbers are compared exactly when the tolerance is 0, otherwise within the
# absolute tolerance. Strings (including digests) are always compared exactly.
# Exit status: 0 if identical, 1 if different, 2 on a usage or reading error.

args <- commandArgs(trailingOnly = TRUE)
tol <- 0
if ("--tolerance" %in% args) {
  i <- match("--tolerance", args)
  tol <- suppressWarnings(as.numeric(args[i + 1L]))
  if (is.na(i) || is.na(tol) || tol < 0) {
    message("compare.R: --tolerance needs a non-negative number")
    quit(save = "no", status = 2L)
  }
  args <- args[-c(i, i + 1L)]
}
if (length(args) != 2L || !all(file.exists(args))) {
  message("usage: Rscript tests/reference/compare.R <a.json> <b.json> [--tolerance 0]")
  if (length(args) == 2L) message("file not found: ", paste(args[!file.exists(args)], collapse = ", "))
  quit(save = "no", status = 2L)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  message("compare.R: jsonlite is required")
  quit(save = "no", status = 2L)
}

read_fp <- function(p) {
  tryCatch(jsonlite::fromJSON(p, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE),
           error = function(e) {
             message("compare.R: cannot parse ", p, ": ", conditionMessage(e))
             quit(save = "no", status = 2L)
           })
}
A <- read_fp(args[[1]])
B <- read_fp(args[[2]])

# ---- helpers ----------------------------------------------------------------

fmt <- function(x) {
  if (length(x) == 0L) return("<empty>")
  if (is.numeric(x)) x <- format(x, digits = 15, trim = TRUE)
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  s <- paste(x, collapse = ", ")
  if (length(x) > 1L) s <- paste0("[", s, "]")
  if (nchar(s) > 90L) s <- paste0(substr(s, 1L, 87L), "...")
  s
}

is_table <- function(x) is.list(x) && !is.null(names(x)) && ".key" %in% names(x) && "data" %in% names(x)

# Differences are collected as character lines; `max_abs` tracks numeric ones.
cmp_atomic <- function(a, b, path) {
  out <- character()
  max_abs <- NA_real_
  if (length(a) == 0L && length(b) == 0L) return(list(lines = out, max_abs = max_abs))
  if (is.numeric(a) && is.numeric(b)) {
    if (length(a) != length(b)) {
      return(list(lines = sprintf("%s: length %d in A, %d in B", path, length(a), length(b)),
                  max_abs = max_abs))
    }
    d <- abs(a - b)
    bad <- (is.na(a) != is.na(b)) | (!is.na(d) & d > tol)
    if (any(bad)) {
      mx <- suppressWarnings(max(d[bad], na.rm = TRUE))
      if (is.finite(mx)) max_abs <- mx
      idx <- which(bad)
      shown <- utils::head(idx, 5L)
      out <- c(out, sprintf("%s: %d of %d values differ%s; A=%s B=%s",
                            path, length(idx), length(a),
                            if (is.finite(mx)) sprintf(" (largest abs diff %s)", format(mx, digits = 6)) else "",
                            fmt(a[shown]), fmt(b[shown])))
    }
  } else if (is.logical(a) && is.logical(b) && length(a) == length(b)) {
    if (!identical(a, b)) out <- sprintf("%s: A=%s B=%s", path, fmt(a), fmt(b))
  } else {
    if (length(a) != length(b) || !identical(as.character(a), as.character(b))) {
      out <- sprintf("%s: A=%s B=%s", path, fmt(a), fmt(b))
    }
  }
  list(lines = out, max_abs = max_abs)
}

cmp_table <- function(a, b, path) {
  lines <- character()
  max_abs <- NA_real_
  upd <- function(m) if (!is.na(m)) max_abs <<- if (is.na(max_abs)) m else max(max_abs, m)
  key_a <- as.character(unlist(a$.key))
  key_b <- as.character(unlist(b$.key))
  if (!identical(key_a, key_b)) {
    return(list(lines = sprintf("%s: key columns differ, A=%s B=%s", path, fmt(key_a), fmt(key_b)),
                max_abs = NA_real_))
  }
  da <- lapply(a$data, function(v) if (is.null(v) || length(v) == 0L) NULL else unlist(v))
  db <- lapply(b$data, function(v) if (is.null(v) || length(v) == 0L) NULL else unlist(v))
  nrow_a <- if (length(da)) max(vapply(da, length, 1L)) else 0L
  nrow_b <- if (length(db)) max(vapply(db, length, 1L)) else 0L
  # NA is read back as NA in vectors, but a column of only nulls is a list; pad.
  getcol <- function(d, cn, n) {
    v <- d[[cn]]
    if (is.null(v)) rep(NA, n) else v
  }
  rowkey <- function(d, n) {
    if (n == 0L) return(character())
    do.call(paste, c(lapply(key_a, function(k) as.character(getcol(d, k, n))), sep = " | "))
  }
  ka <- rowkey(da, nrow_a)
  kb <- rowkey(db, nrow_b)
  only_a <- setdiff(ka, kb)
  only_b <- setdiff(kb, ka)
  if (length(only_a)) lines <- c(lines, sprintf("%s: rows only in A: %s", path, fmt(only_a)))
  if (length(only_b)) lines <- c(lines, sprintf("%s: rows only in B: %s", path, fmt(only_b)))
  cols_a <- setdiff(names(da), key_a)
  cols_b <- setdiff(names(db), key_a)
  if (!setequal(cols_a, cols_b)) {
    lines <- c(lines, sprintf("%s: columns differ, only in A: %s; only in B: %s", path,
                              fmt(setdiff(cols_a, cols_b)), fmt(setdiff(cols_b, cols_a))))
  }
  common <- intersect(ka, kb)
  for (cn in intersect(cols_a, cols_b)) {
    va <- getcol(da, cn, nrow_a)[match(common, ka)]
    vb <- getcol(db, cn, nrow_b)[match(common, kb)]
    if (is.logical(va) && all(is.na(va))) va <- as.numeric(va)
    if (is.logical(vb) && all(is.na(vb))) vb <- as.numeric(vb)
    if (is.numeric(va) && is.numeric(vb)) {
      d <- abs(va - vb)
      bad <- (is.na(va) != is.na(vb)) | (!is.na(d) & d > tol)
    } else {
      bad <- as.character(va) != as.character(vb)
      bad[is.na(bad)] <- TRUE
    }
    for (i in which(bad)) {
      lines <- c(lines, sprintf("%s [%s] %s: A=%s B=%s", path, common[i], cn, fmt(va[i]), fmt(vb[i])))
    }
    if (is.numeric(va) && is.numeric(vb) && any(bad)) {
      mx <- suppressWarnings(max(d[bad], na.rm = TRUE))
      if (is.finite(mx)) upd(mx)
    }
  }
  # Digest differences alone (e.g. only key order) are still worth saying.
  if (!length(lines) && !identical(a$.digest, b$.digest) && tol == 0) {
    lines <- sprintf("%s: table digest differs although no cell differs (row or column order?)", path)
  }
  list(lines = lines, max_abs = max_abs)
}

cmp <- function(a, b, path) {
  if (is_table(a) && is_table(b)) return(cmp_table(a, b, path))
  if (is.list(a) && is.list(b)) {
    na <- names(a); nb <- names(b)
    if (!is.null(na) && !is.null(nb)) {
      lines <- character(); max_abs <- NA_real_
      only_a <- setdiff(na, nb); only_b <- setdiff(nb, na)
      for (n in only_a) lines <- c(lines, sprintf("%s/%s: missing in B", path, n))
      for (n in only_b) lines <- c(lines, sprintf("%s/%s: missing in A", path, n))
      for (n in intersect(na, nb)) {
        r <- cmp(a[[n]], b[[n]], paste0(path, "/", n))
        lines <- c(lines, r$lines)
        if (!is.na(r$max_abs)) max_abs <- if (is.na(max_abs)) r$max_abs else max(max_abs, r$max_abs)
      }
      return(list(lines = lines, max_abs = max_abs))
    }
    # Unnamed lists (e.g. empty arrays): compare their flattened content.
    return(cmp_atomic(unlist(a), unlist(b), path))
  }
  if (is.list(a) || is.list(b)) {
    # A list on one side and an atomic value on the other: empty arrays read as list().
    la <- if (is.list(a)) unlist(a) else a
    lb <- if (is.list(b)) unlist(b) else b
    if (length(la) == 0L && length(lb) == 0L) return(list(lines = character(), max_abs = NA_real_))
    return(cmp_atomic(la, lb, path))
  }
  cmp_atomic(a, b, path)
}

# ---- report -----------------------------------------------------------------

sections <- union(names(A), names(B))
n_diff <- 0L
details <- list()
max_lines <- 25L

cat(sprintf("compare.R: A = %s\n           B = %s\n           tolerance = %s\n\n",
            basename(args[[1]]), basename(args[[2]]), format(tol)))

for (s in sections) {
  if (is.null(A[[s]]) && !(s %in% names(A))) {
    r <- list(lines = sprintf("%s: section missing in A", s), max_abs = NA_real_)
  } else if (is.null(B[[s]]) && !(s %in% names(B))) {
    r <- list(lines = sprintf("%s: section missing in B", s), max_abs = NA_real_)
  } else {
    r <- cmp(A[[s]], B[[s]], s)
  }
  if (length(r$lines) == 0L) {
    cat(sprintf("%-24s IDENTICAL\n", s))
  } else {
    n_diff <- n_diff + 1L
    cat(sprintf("%-24s DIFFERENT\n", s))
    details[[s]] <- r
  }
}

for (s in names(details)) {
  r <- details[[s]]
  cat(sprintf("\n-- %s: %d difference(s)", s, length(r$lines)))
  if (!is.na(r$max_abs)) cat(sprintf("; largest absolute numeric difference %s", format(r$max_abs, digits = 6)))
  cat("\n")
  shown <- utils::head(r$lines, max_lines)
  cat(paste0("   ", shown), sep = "\n")
  if (length(r$lines) > max_lines) cat(sprintf("   ... and %d more\n", length(r$lines) - max_lines))
}

cat("\n")
if (n_diff == 0L) {
  cat("RESULT: IDENTICAL\n")
  quit(save = "no", status = 0L)
} else {
  cat(sprintf("RESULT: DIFFERENT (%d sections)\n", n_diff))
  quit(save = "no", status = 1L)
}
