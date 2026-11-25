# Greek letters in figure labels
#
# R's pdf() device cannot draw Greek letters typed as text (they come out as
# dots), and cairo_pdf() does not suit Affinity. plotmath can: it draws them
# with the standard PDF Symbol font. So a name such as IFN- followed by a
# Greek gamma becomes the plotmath expression "IFN-"*gamma, only where a
# figure draws it.
# Matching, file names and tables keep the name as typed.
#
# Where a figure cannot take plotmath (CATALYST's own heatmaps), the letter is
# spelt out instead ("IFN-gamma"), so nothing prints as a dot.

# The letters plotmath knows, by their code points (the file stays ASCII)
.greek_letters <- stats::setNames(
  c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
    "iota", "kappa", "lambda", "mu", "mu", "nu", "xi", "omicron", "pi", "rho",
    "sigma", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega",
    "Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta",
    "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho",
    "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega"),
  intToUtf8(c(0x3b1:0x3bc, 0xb5, 0x3bd:0x3c1, 0x3c3, 0x3c2, 0x3c4:0x3c9,
              0x391:0x3a1, 0x3a3:0x3a9), multiple = TRUE)
)

.has_greek <- function(x) {
  !is.na(x) & grepl(paste0("[", paste(names(.greek_letters), collapse = ""), "]"), x)
}

# One name as a plotmath call: runs of text as strings, letters as symbols,
# joined with * (which plotmath draws with no space)
.greek_call <- function(s) {
  chars <- strsplit(s, "")[[1]]
  is_g <- chars %in% names(.greek_letters)
  run <- cumsum(c(TRUE, diff(is_g) != 0 | is_g[-1]))
  parts <- lapply(split(seq_along(chars), run), function(i) {
    if (is_g[i[1]]) as.name(.greek_letters[[chars[i[1]]]]) else paste(chars[i], collapse = "")
  })
  Reduce(function(a, b) call("*", a, b), parts)
}

#' Figure labels that draw Greek letters
#'
#' Names with a Greek letter become plotmath expressions, which every R
#' graphics device draws, `pdf()` included (with the Symbol font). Other names
#' are kept as they are. Use it wherever a figure takes labels: `labels =` in
#' ggplot2 scales, `labs()`, facet labellers, or `row_labels` and
#' `column_labels` in ComplexHeatmap.
#'
#' @param x Names (a character vector, or something that becomes one).
#' @param bold Draw names with a Greek letter in bold, for titles and strips
#'   (plotmath ignores the theme's bold).
#' @return An expression vector as long as `x` when any name has a Greek
#'   letter; otherwise `x` as a character vector, unchanged.
#' @export
greek_label <- function(x, bold = FALSE) {
  x <- as.character(x)
  g <- .has_greek(x)
  if (!any(g)) return(x)
  out <- as.expression(as.list(ifelse(is.na(x), "NA", x)))
  for (i in which(g)) out[[i]] <- if (bold) call("bold", .greek_lines(x[[i]])) else .greek_lines(x[[i]])
  out
}

# Several lines (split at "\n") are stacked with plotmath's atop()
.greek_lines <- function(s) {
  lines <- trimws(strsplit(s, "\n", fixed = TRUE)[[1]])
  calls <- lapply(lines, function(l) if (nzchar(l)) .greek_call(l) else "")
  if (length(calls) == 1) return(calls[[1]])
  # displaystyle() keeps both lines full size (atop alone shrinks them)
  calls <- lapply(calls, function(cl) call("displaystyle", cl))
  Reduce(function(a, b) call("atop", a, b), calls, right = TRUE)
}

#' Names with Greek letters spelt out
#'
#' For figures that cannot draw Greek letters in a PDF: each Greek letter is
#' written out as its name (a gamma becomes "gamma"), so nothing prints as a
#' dot.
#'
#' @param x Names.
#' @return `x` with each Greek letter replaced by its name.
#' @export
spell_greek <- function(x) {
  x <- as.character(x)
  for (g in names(.greek_letters)) x <- gsub(g, .greek_letters[[g]], x, fixed = TRUE)
  x
}

#' A marker's display name, as plain text
#'
#' @param x Marker names, as used in the data.
#' @param display A named character vector: display name, named by marker.
#'   Markers without one keep their own name. `NULL` for none.
#' @return The display names (character), for titles that combine names.
#' @export
marker_display <- function(x, display = NULL) {
  x <- as.character(x)
  if (length(display)) {
    shown <- unname(display[x])
    use <- !is.na(shown) & nzchar(trimws(shown))
    x[use] <- shown[use]
  }
  x
}

#' Labels for markers, using their display names
#'
#' @inheritParams marker_display
#' @inheritParams greek_label
#' @return The display names, ready for figures (see [greek_label()]).
#' @export
marker_label <- function(x, display = NULL, bold = FALSE) {
  greek_label(marker_display(x, display), bold = bold)
}

#' A ggplot2 facet labeller that draws Greek letters
#'
#' Like [ggplot2::label_value()], but strip labels with a Greek letter are
#' drawn with plotmath (see [greek_label()]).
#'
#' @param labels A data frame of facet values, as ggplot2 passes it.
#' @param multi_line Kept for ggplot2; each variable gets its own line.
#' @return A list of label lists, as ggplot2 labellers return.
#' @export
label_greek <- function(labels, multi_line = TRUE) {
  lapply(labels, function(v) lapply(as.character(v), function(s) {
    e <- greek_label(s, bold = TRUE)
    if (is.expression(e)) e else as.expression(s)
  }))
}

#' Text labels that draw Greek letters
#'
#' For text drawn from data, such as `geom_text()` or
#' `ggrepel::geom_label_repel()` labels: returns each name as plotmath text, to
#' draw with `parse = TRUE`. Names without a Greek letter come back quoted, so
#' they are drawn exactly as they are.
#'
#' @param x Names.
#' @return A character vector of plotmath text, one per name.
#' @export
greek_parse_text <- function(x) {
  e <- greek_label(x)
  if (!is.expression(e)) e <- as.expression(as.list(ifelse(is.na(e), "NA", e)))
  vapply(seq_along(e), function(i) paste(deparse(e[[i]]), collapse = ""), "")
}

#' A facet labeller from prepared labels, drawing Greek letters
#'
#' For strips whose text is made beforehand, for example a condition with its
#' number of cells on a second line. Values missing from `lookup` show as
#' they are.
#'
#' @param lookup A named character vector: the label, named by facet value.
#' @return A ggplot2 labeller.
#' @export
greek_labeller <- function(lookup) {
  if (is.null(lookup)) lookup <- character()
  function(labels, multi_line = TRUE) {
    lapply(labels, function(v) {
      v <- as.character(v)
      shown <- unname(lookup[v])
      shown[is.na(shown)] <- v[is.na(shown)]
      e <- greek_label(shown, bold = TRUE)
      # Plain text, as ggplot2's own labellers give it, unless a letter is Greek
      if (is.expression(e)) lapply(seq_along(e), function(i) e[i]) else shown
    })
  }
}

#' Greek letters in a finished heatmap
#'
#' Takes a ComplexHeatmap heatmap (from MARMOT, CATALYST or `Heatmap()`) and
#' swaps in labels that draw Greek letters: row and column names, and the
#' legends of its annotations (conditions, clusters). Row or column names that
#' are markers show their display names. Nothing in the data changes, and a
#' heatmap without Greek letters or display names comes back untouched.
#'
#' @param h A `Heatmap` or `HeatmapList`.
#' @param display Display names for markers, as for [marker_label()].
#' @return `h`, relabelled.
#' @export
greek_heatmap <- function(h, display = NULL) {
  if (inherits(h, "HeatmapList")) {
    h@ht_list <- lapply(h@ht_list, function(x) if (inherits(x, "Heatmap")) greek_heatmap(x, display) else x)
    return(h)
  }
  if (!inherits(h, "Heatmap")) return(h)

  relabel <- function(x) {
    if (is.null(x) || !length(x)) return(NULL)
    x <- x0 <- as.character(x)
    # CATALYST's DS rows read "marker(cluster)": the marker part gets its display name
    for (mk in names(display)[!is.na(display) & nzchar(trimws(display))]) {
      hit <- startsWith(x, paste0(mk, "("))
      x[hit] <- paste0(display[[mk]], substring(x[hit], nchar(mk) + 1))
    }
    e <- marker_label(x, display)
    if (is.expression(e) || !identical(e, x0)) e else NULL
  }

  # Row and column names: rebuild the text annotation that draws them
  for (dim in c("row", "column")) {
    p <- methods::slot(h, paste0(dim, "_names_param"))
    new <- relabel(p$labels)
    if (is.null(new) || is.null(p$anno)) next
    near <- p$side %in% c("left", "bottom")   # text runs towards the heatmap
    p$labels <- new
    p$anno <- ComplexHeatmap::anno_text(
      new, which = dim, gp = p$gp, rot = p$rot,
      just = if (isTRUE(p$centered)) "centre" else if (near) "right" else "left",
      location = if (isTRUE(p$centered)) grid::unit(0.5, "npc") else grid::unit(if (near) 1 else 0, "npc"))
    methods::slot(h, paste0(dim, "_names_param")) <- p
  }

  # Annotation legends (conditions, clusters): Greek labels for their levels
  for (side in c("top_annotation", "bottom_annotation", "left_annotation", "right_annotation")) {
    ha <- methods::slot(h, side)
    if (is.null(ha) || !methods::is(ha, "HeatmapAnnotation")) next
    for (nm in names(ha@anno_list)) {
      a <- ha@anno_list[[nm]]
      cm <- a@color_mapping
      if (is.null(cm) || !methods::is(cm, "ColorMapping") || cm@type != "discrete") next
      lab <- greek_label(cm@levels)
      if (!is.expression(lab)) next
      a@legend_param$labels <- lab
      ha@anno_list[[nm]] <- a
    }
    methods::slot(h, side) <- ha
  }
  h
}
