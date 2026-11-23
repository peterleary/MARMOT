# Label, then regenerate
#
# After a first run, MARMOT writes MARMOT_Cluster_Labels.xlsx into the results
# folder: one row per cluster with its size and its most distinctive markers,
# and an empty Label column. The user names clusters there and reruns from
# those results; the report then adds figures and DA/DS for the named
# populations without recomputing QC, clustering or embeddings.
#
# Labels follow MARMOT's name rule: two labels that differ only in capitals,
# spaces, hyphens or underscores are the same label, so "T cells" and
# "T-cells" merge. A blank label keeps the cluster as it is; "drop" removes it.

#' Write the cluster labels workbook
#'
#' @param path Where to write the workbook.
#' @param clusters The cluster of each cell (a factor).
#' @param expr A markers x cells matrix of transformed values, used to name
#'   each cluster's most distinctive markers.
#' @param n_top How many distinctive markers to list per cluster.
#' @return `path`, invisibly.
#' @export
write_cluster_labels_template <- function(path, clusters, expr, n_top = 3) {
  clusters <- droplevels(as.factor(clusters))
  idx <- split(seq_along(clusters), clusters)
  med <- vapply(idx, function(i) apply(expr[, i, drop = FALSE], 1, stats::median), numeric(nrow(expr)))
  med <- matrix(med, nrow = nrow(expr), dimnames = list(rownames(expr), names(idx)))
  z <- t(scale(t(med)))
  z[is.na(z)] <- 0
  top <- vapply(colnames(z), function(cl) {
    o <- order(z[, cl], decreasing = TRUE)[seq_len(min(n_top, nrow(z)))]
    paste(rownames(z)[o][z[o, cl] > 0.5], collapse = ", ")
  }, character(1))
  sizes <- lengths(idx)
  tab <- data.frame(
    Cluster = names(idx),
    Cells = as.integer(sizes),
    `Share of cells (%)` = round(100 * sizes / sum(sizes), 1),
    `Most distinctive markers` = unname(top),
    Label = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  readme <- data.frame(`How to use this sheet` = c(
    "Type a name for any cluster in the Label column, then run MARMOT again with RDataFolder set to these results.",
    "Clusters with the same name are merged into one population.",
    "Leave a label blank to keep the cluster as it is.",
    "Type drop to leave a cluster out (for small, clearly noisy islands). Frequencies are then out of the remaining cells, and the report says how many cells this removed from each sample.",
    "Capitals, spaces, hyphens and underscores do not matter: 'T cells' and 'T-cells' are the same label."
  ), check.names = FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Cluster Labels")
  openxlsx::writeData(wb, "Cluster Labels", tab)
  openxlsx::setColWidths(wb, "Cluster Labels", cols = 1:5, widths = c(10, 10, 18, 40, 30))
  openxlsx::addStyle(wb, "Cluster Labels", openxlsx::createStyle(textDecoration = "bold"), rows = 1, cols = 1:5)
  openxlsx::freezePane(wb, "Cluster Labels", firstRow = TRUE)
  openxlsx::addWorksheet(wb, "Read me")
  openxlsx::writeData(wb, "Read me", readme)
  openxlsx::setColWidths(wb, "Read me", cols = 1, widths = 120)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Read the cluster labels workbook
#'
#' @param path The workbook written by [write_cluster_labels_template()].
#' @param clusters The clusters of the results being labelled (their names).
#' @return `NULL` when no cluster has a label. Otherwise a list with `labels`,
#'   a named character vector giving each cluster's population (its own name
#'   when unlabelled, `NA` when dropped), `dropped` (the dropped clusters),
#'   `notes` (plain sentences for the report) and `unknown` (clusters in the
#'   sheet that are not in the results).
#' @export
read_cluster_labels <- function(path, clusters) {
  clusters <- as.character(clusters)
  tab <- openxlsx::read.xlsx(path, sheet = "Cluster Labels", check.names = FALSE, sep.names = " ")
  tab$Cluster <- as.character(tab$Cluster)
  tab$Label <- trimws(as.character(tab$Label))
  tab$Label[is.na(tab$Label)] <- ""
  if (!any(nzchar(tab$Label))) return(NULL)

  unknown <- setdiff(tab$Cluster[nzchar(tab$Label)], clusters)
  tab <- tab[tab$Cluster %in% clusters, , drop = FALSE]
  key <- name_key(tab$Label)
  is_drop <- key == "DROP"

  # One spelling per label: the first one typed
  first_spelling <- tab$Label[match(key, key)]
  labels <- stats::setNames(clusters, clusters)
  labelled <- nzchar(tab$Label) & !is_drop
  labels[tab$Cluster[labelled]] <- first_spelling[labelled]
  labels[tab$Cluster[is_drop]] <- NA_character_

  notes <- character()
  respelt <- labelled & tab$Label != first_spelling
  for (s in unique(tab$Label[respelt])) {
    notes <- c(notes, sprintf("The label '%s' was taken as '%s'.", s, first_spelling[match(s, tab$Label)]))
  }
  merged <- table(labels[!is.na(labels) & labels != names(labels)])
  for (lab in names(merged)[merged > 1]) {
    notes <- c(notes, sprintf("Clusters %s were merged into '%s'.",
                              paste(names(labels)[labels %in% lab], collapse = ", "), lab))
  }
  if (length(unknown)) {
    notes <- c(notes, sprintf("The sheet labels cluster(s) %s, which are not in these results; those labels were ignored.",
                              paste(unknown, collapse = ", ")))
  }
  list(labels = labels, dropped = tab$Cluster[is_drop], notes = notes, unknown = unknown)
}
