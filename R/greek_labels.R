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
#' @return An expression vector as long as `x` when any name has a Greek
#'   letter; otherwise `x` as a character vector, unchanged.
#' @export
greek_label <- function(x) {
  x <- as.character(x)
  g <- .has_greek(x)
  if (!any(g)) return(x)
  out <- as.expression(as.list(ifelse(is.na(x), "NA", x)))
  for (i in which(g)) out[[i]] <- .greek_call(x[[i]])
  out
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

#' Labels for markers, using their display names
#'
#' @param x Marker names, as used in the data.
#' @param display A named character vector: display name, named by marker.
#'   Markers without one keep their own name. `NULL` for none.
#' @return The display names, ready for figures (see [greek_label()]).
#' @export
marker_label <- function(x, display = NULL) {
  x <- as.character(x)
  if (length(display)) {
    shown <- display[x]
    use <- !is.na(shown) & nzchar(trimws(shown))
    x[use] <- shown[use]
  }
  greek_label(x)
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
    e <- greek_label(s)
    if (is.expression(e)) e else as.expression(s)
  }))
}
