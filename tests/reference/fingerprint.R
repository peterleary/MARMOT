#!/usr/bin/env Rscript
# fingerprint.R -- reduce one MARMOT results folder to a compact JSON summary of
# every number that matters, so that two runs can be compared with compare.R.
#
# Usage:
#   Rscript tests/reference/fingerprint.R <results_dir> <out.json>
#
# <results_dir> is a "Results_Files_*" folder (containing R_files/ and
# Excel_Files/) or the R_files folder itself. Works from any working directory.
#
# Schema version 1. Tables are written as
#   list(.key = <key columns>, .digest = <hash>, data = <columns>)
# so that compare.R can match rows by key and report them individually.

FINGERPRINT_SCHEMA <- 1L

fail <- function(...) {
  message("fingerprint.R: ", ...)
  quit(save = "no", status = 1L)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  fail("usage: Rscript tests/reference/fingerprint.R <results_dir> <out.json>")
}
in_path <- args[[1]]
out_json <- args[[2]]

if (!dir.exists(in_path)) fail("folder not found: ", in_path)
in_path <- normalizePath(in_path, mustWork = TRUE)

# Accept either the results folder or its R_files subfolder.
if (basename(in_path) == "R_files" ||
    (file.exists(file.path(in_path, "marmot_results.h5ad")) &&
       !dir.exists(file.path(in_path, "R_files")))) {
  r_dir <- in_path
  results_dir <- dirname(in_path)
} else {
  r_dir <- file.path(in_path, "R_files")
  results_dir <- in_path
}
h5ad_path <- file.path(r_dir, "marmot_results.h5ad")
if (!dir.exists(r_dir)) fail("R_files folder not found under: ", in_path)
if (!file.exists(h5ad_path)) fail("marmot_results.h5ad not found in: ", r_dir)
out_dir <- dirname(out_json)
if (!dir.exists(out_dir)) fail("output folder does not exist: ", out_dir)

suppressPackageStartupMessages({
  library(SingleCellExperiment)
  library(SummarizedExperiment)
  library(S4Vectors)
})
for (pkg in c("MARMOT", "jsonlite", "qs2", "rlang", "openxlsx")) {
  if (!requireNamespace(pkg, quietly = TRUE)) fail("required package not installed: ", pkg)
}

# ---- helpers ----------------------------------------------------------------

# Order labels naturally, so that "2" < "10" and "C2" < "C10".
natural_order <- function(...) {
  keys <- list(...)
  pad <- function(x) {
    x <- as.character(x)
    vapply(x, function(s) {
      if (is.na(s)) return("￿")
      m <- gregexpr("[0-9]+", s)[[1]]
      if (m[1] == -1L) return(s)
      pieces <- regmatches(s, gregexpr("[0-9]+", s))[[1]]
      rest <- regmatches(s, gregexpr("[0-9]+", s), invert = TRUE)[[1]]
      out <- rest[1]
      for (i in seq_along(pieces)) {
        out <- paste0(out, formatC(as.numeric(pieces[i]), width = 15, flag = "0", format = "f", digits = 0), rest[i + 1])
      }
      out
    }, character(1), USE.NAMES = FALSE)
  }
  do.call(order, lapply(keys, pad))
}

named_list <- function(x) {
  # Named vector -> named list (JSON object), keeping order.
  stats::setNames(as.list(unname(x)), names(x))
}

# Build a table node. `numeric_cols` are rounded to 10 significant digits.
make_table <- function(df, key, numeric_cols = character()) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  keep <- intersect(c(key, numeric_cols), colnames(df))
  df <- df[, keep, drop = FALSE]
  for (cn in colnames(df)) {
    if (cn %in% key) {
      df[[cn]] <- as.character(df[[cn]])
    } else {
      df[[cn]] <- signif(as.numeric(df[[cn]]), 10)
    }
  }
  key <- intersect(key, colnames(df))
  if (nrow(df) > 0L && length(key) > 0L) {
    df <- df[do.call(natural_order, unname(as.list(df[key]))), , drop = FALSE]
  }
  rownames(df) <- NULL
  list(
    .key = I(key),
    .digest = rlang::hash(df),
    data = as.list(df)
  )
}

safe_name <- function(x) gsub("[^a-zA-Z0-9._-]", "_", x)

# ---- load the h5ad, capturing warnings --------------------------------------

env <- new.env()
warn_msgs <- character()
withCallingHandlers(
  MARMOT::load_h5ad_to_env(h5ad_path, envir = env),
  warning = function(w) {
    warn_msgs <<- c(warn_msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  },
  message = function(m) invokeRestart("muffleMessage")
)
sce <- env$sce
if (is.null(sce)) fail("no SingleCellExperiment could be reconstructed from: ", h5ad_path)

load_warnings <- list(
  total = length(warn_msgs),
  distinct = lapply(
    split(seq_along(warn_msgs), warn_msgs) |> lapply(length),
    function(n) n
  )
)
if (length(warn_msgs) == 0L) load_warnings$distinct <- list()

# ---- meta -------------------------------------------------------------------

settings_path <- file.path(r_dir, "pipeline_settings.json")
settings <- if (file.exists(settings_path)) {
  jsonlite::fromJSON(settings_path, simplifyVector = FALSE)
} else {
  "pipeline_settings.json not found"
}
meta <- list(
  # Strip the trailing run timestamp (_YYYY-MM-DD_HH.MM.SS) so reruns compare equal.
  results_dir = sub("_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}$", "", basename(results_dir)),
  marmot_version = as.character(utils::packageVersion("MARMOT")),
  fingerprint_schema = FINGERPRINT_SCHEMA,
  pipeline_settings = settings
)

# ---- cells ------------------------------------------------------------------

cd <- as.data.frame(colData(sce))
sid <- cd$sample_id
sid_f <- if (is.factor(sid)) sid else factor(sid, levels = unique(sid))
cells <- list(
  total = ncol(sce),
  per_sample = named_list(table(sid_f))
)
cells$per_sample <- lapply(cells$per_sample, as.integer)

# ---- assays -----------------------------------------------------------------

assays_fp <- lapply(stats::setNames(assayNames(sce), assayNames(sce)), function(an) {
  m <- as.matrix(assay(sce, an))
  ms <- signif(m, 10)
  list(
    dim = dim(m),
    digest = rlang::hash(list(dimnames(ms), ms)),
    grand_sum = sum(m, na.rm = TRUE),
    marker_means = named_list(signif(rowMeans(m, na.rm = TRUE), 10))
  )
})

# ---- clusterings ------------------------------------------------------------

clus_cols <- intersect(
  colnames(cd),
  c("cluster_id", grep("^(k|p|meta)[0-9]+$", colnames(cd), value = TRUE))
)
clusterings <- lapply(stats::setNames(clus_cols, clus_cols), function(cn) {
  x <- cd[[cn]]
  lev <- if (is.factor(x)) levels(x) else unique(as.character(x))
  lev <- lev[natural_order(lev)]
  tab <- table(factor(as.character(x), levels = lev))
  list(
    n_levels = length(lev),
    sizes = lapply(named_list(tab), as.integer),
    labels_digest = rlang::hash(as.character(x))
  )
})

# ---- cluster x sample table -------------------------------------------------

cluster_by_sample <- NULL
if ("cluster_id" %in% colnames(cd)) {
  tab <- table(
    cluster_id = as.character(cd$cluster_id),
    sample_id = as.character(sid_f)
  )
  df <- data.frame(cluster_id = rownames(tab), stringsAsFactors = FALSE)
  for (s in levels(sid_f)) {
    df[[s]] <- if (s %in% colnames(tab)) as.integer(tab[, s]) else 0L
  }
  cluster_by_sample <- make_table(df, key = "cluster_id", numeric_cols = levels(sid_f))
}

# ---- embeddings -------------------------------------------------------------

embeddings <- lapply(stats::setNames(reducedDimNames(sce), reducedDimNames(sce)), function(rn) {
  m <- as.matrix(reducedDim(sce, rn))
  r <- round(m, 6)
  dimnames(r) <- NULL
  first_two <- seq_len(min(2L, ncol(m)))
  list(
    dim = dim(m),
    digest = rlang::hash(r),
    ranges = lapply(stats::setNames(first_two, paste0("col", first_two)), function(j) {
      range(r[, j], na.rm = TRUE)
    })
  )
})

# ---- DA / DS / selected clusters --------------------------------------------

da_tables <- function(lst) {
  if (is.null(lst)) return("absent")
  out <- lapply(lst, function(df) {
    make_table(df, key = "cluster_id", numeric_cols = c("logFC", "p_val", "p_adj"))
  })
  names(out) <- names(lst)
  out
}
ds_tables <- function(lst) {
  if (is.null(lst)) return("absent")
  out <- lapply(lst, function(x) {
    df <- if (is.data.frame(x)) x else if (is.list(x) && !is.null(x$tbl_DS)) x$tbl_DS else NULL
    if (is.null(df)) return("no table found")
    make_table(df, key = c("cluster_id", "marker_id"),
               numeric_cols = c("logFC", "p_val", "p_adj"))
  })
  names(out) <- names(lst)
  out
}
selected_node <- function(lst) {
  if (is.null(lst)) return("absent")
  out <- lapply(lst, function(x) I(as.character(x)))
  names(out) <- names(lst)
  out
}

qs2_path <- file.path(r_dir, "da_ds_results.qs2")
qs2_res <- if (file.exists(qs2_path)) qs2::qs_read(qs2_path) else NULL
# Only the DA/DS elements are used from the qs2 file; the embedded sceDS object
# is deliberately ignored (raw-data-sized, covered by the h5ad assays).

da_qs2 <- da_tables(qs2_res$daList)
da_h5ad <- da_tables(env$daList)
ds_qs2 <- ds_tables(qs2_res$dsList)
ds_h5ad <- ds_tables(env$dsList)

# Agreement between the two stores, matched by sanitised contrast name. The h5ad
# drops empty selected-cluster entries, so only non-empty ones are compared.
agree <- function(a, b) {
  if (identical(a, "absent") || identical(b, "absent")) return("one store absent")
  names(a) <- safe_name(names(a))
  names(b) <- safe_name(names(b))
  all_names <- union(names(a), names(b))
  stats::setNames(lapply(all_names, function(n) {
    if (is.null(a[[n]]) || is.null(b[[n]])) "present in one store only"
    else identical(a[[n]]$.digest, b[[n]]$.digest)
  }), all_names)
}
sel_qs2 <- selected_node(qs2_res$selectedClustersList)
sel_h5ad <- selected_node(env$selectedClustersList)
nonempty <- function(x) if (identical(x, "absent")) x else Filter(function(v) length(v) > 0L, x)
store_agreement <- list(
  da = agree(da_qs2, da_h5ad),
  ds = agree(ds_qs2, ds_h5ad),
  selected_clusters = if (identical(sel_qs2, "absent") || identical(sel_h5ad, "absent")) {
    "one store absent"
  } else {
    identical(lapply(nonempty(sel_qs2), as.character), lapply(nonempty(sel_h5ad), as.character))
  }
)

# ---- Excel ------------------------------------------------------------------

excel_dir <- file.path(results_dir, "Excel_Files")
big_bytes <- 20e6
excel <- list()
if (dir.exists(excel_dir)) {
  xfiles <- sort(list.files(excel_dir, pattern = "\\.xlsx$", full.names = TRUE))
  for (f in xfiles) {
    sheets <- openxlsx::getSheetNames(f)
    node <- list(sheets = I(sheets))
    if (file.size(f) > big_bytes) {
      node$note <- "workbook too large to read quickly; sheet names only (dimensions and digests not recorded)"
    } else {
      per_sheet <- lapply(stats::setNames(sheets, sheets), function(s) {
        d <- tryCatch(
          openxlsx::read.xlsx(f, sheet = s, colNames = FALSE,
                              skipEmptyRows = FALSE, skipEmptyCols = FALSE),
          error = function(e) NULL
        )
        if (is.null(d)) return(list(note = "sheet could not be read (empty?)"))
        chr <- lapply(d, as.character)
        list(dim = dim(d), digest = rlang::hash(chr))
      })
      node$sheet_content <- per_sheet
    }
    excel[[basename(f)]] <- node
  }
}

# ---- assemble and write ------------------------------------------------------

fp <- list(
  meta = meta,
  cells = cells,
  assays = assays_fp,
  clusterings = clusterings,
  cluster_by_sample = cluster_by_sample,
  embeddings = embeddings,
  da_qs2 = da_qs2,
  da_h5ad = da_h5ad,
  ds_qs2 = ds_qs2,
  ds_h5ad = ds_h5ad,
  selected_clusters_qs2 = sel_qs2,
  selected_clusters_h5ad = sel_h5ad,
  store_agreement = store_agreement,
  excel = excel,
  load_warnings = load_warnings
)

json <- jsonlite::toJSON(fp, pretty = TRUE, auto_unbox = TRUE, digits = NA,
                         na = "null", null = "null")
writeLines(json, out_json, useBytes = TRUE)
message("fingerprint written: ", basename(out_json))
quit(save = "no", status = 0L)
