# Forgiving name matching
#
# Users type names in the metadata sheet (markers, conditions, file names) that
# must match names in their FCS files or elsewhere in the sheet. Two names count
# as the same when they are equal after ignoring capitals, spaces, hyphens and
# underscores: "Il7R", "IL-7R", "IL 7R" and "il_7r" all match "IL7R". MARMOT
# never matches silently: every forgiving match is returned so the report can
# say what was done.

#' The key two names are compared by
#'
#' Upper case, with spaces, hyphens and underscores removed. Two names with
#' the same key count as the same name.
#'
#' @param x A character vector.
#' @return A character vector of keys, the same length as `x`.
#' @export
name_key <- function(x) {
  toupper(gsub("[[:space:]_-]", "", as.character(x)))
}

#' Match names forgivingly
#'
#' Finds each wanted name among the available names, in three tiers:
#' an exact match is used as it is; failing that, a single match that is
#' equal after ignoring capitals, spaces, hyphens and underscores is used;
#' if several available names match that way, the one that differs only in
#' capitals is preferred, and otherwise the first. Everything but an exact
#' match is reported.
#'
#' @param wanted Names to look for (for example, those typed in the sheet).
#' @param available Names to look in (for example, those in an FCS file).
#' @return A list with `matched`, the available name used for each wanted
#'   name (`NA` when nothing matches), and `notes`, a data frame with one row
#'   for each wanted name that was not matched exactly: `wanted`, `used`
#'   (`NA` when nothing matched), `how` ("forgiving", "chosen" or "missing")
#'   and `candidates` (all the available names that matched, comma-separated).
#' @export
match_names <- function(wanted, available) {
  wanted <- as.character(wanted); available <- as.character(available)
  matched <- rep(NA_character_, length(wanted))
  notes <- data.frame(wanted = character(), used = character(), how = character(),
                      candidates = character(), stringsAsFactors = FALSE)
  akey <- name_key(available)
  for (i in seq_along(wanted)) {
    w <- wanted[[i]]
    if (is.na(w)) next
    if (w %in% available) { matched[i] <- w; next }
    hits <- unique(available[akey == name_key(w)])
    if (length(hits) == 0) {
      notes[nrow(notes) + 1, ] <- list(w, NA_character_, "missing", "")
    } else if (length(hits) == 1) {
      matched[i] <- hits
      notes[nrow(notes) + 1, ] <- list(w, hits, "forgiving", hits)
    } else {
      case_only <- hits[toupper(hits) == toupper(w)]
      matched[i] <- if (length(case_only)) case_only[[1]] else hits[[1]]
      notes[nrow(notes) + 1, ] <- list(w, matched[i], "chosen", paste(hits, collapse = ", "))
    }
  }
  list(matched = matched, notes = notes)
}

#' Describe forgiving matches in plain words
#'
#' Turns the `notes` of [match_names()] into one sentence per note, for the
#' report and the run log.
#'
#' @param notes The `notes` data frame from [match_names()].
#' @param what What the names are, such as "marker" or "condition".
#' @param where Where the available names came from, such as "the FCS files".
#' @return A character vector of sentences (empty when there is nothing to say).
#' @export
describe_name_matches <- function(notes, what, where) {
  if (is.null(notes) || !nrow(notes)) return(character())
  vapply(seq_len(nrow(notes)), function(i) {
    n <- notes[i, ]
    switch(n$how,
      forgiving = sprintf("The %s '%s' was matched to '%s' in %s (capitals, spaces, hyphens and underscores are ignored).",
                          what, n$wanted, n$used, where),
      chosen    = sprintf("The %s '%s' could be any of %s in %s; '%s' was used. To use another, type its exact spelling in the sheet.",
                          what, n$wanted, paste0("'", strsplit(n$candidates, ", ", fixed = TRUE)[[1]], "'", collapse = ", "),
                          where, n$used),
      missing   = sprintf("The %s '%s' was not found in %s.", what, n$wanted, where)
    )
  }, character(1))
}
