# Lining up FCS files that were written differently
#
# Files exported on different days, or from different datasets, often name the
# same marker differently ("IL7R" in one export, "Il7R" in the next), list the
# channels in a different order, or put a marker on a different channel. The
# pipeline stacks every file into one flowSet, which needs identical channel
# names in the same order. These helpers work out, from the file headers alone,
# how each file's channels line up with the first file's, so that every file
# can be renamed and reordered as it is read. Markers are matched with
# match_names(): capitals, spaces, hyphens and underscores are ignored.
# Everything that was lined up is described in plain words for the report.

# The channels and markers of each file, read from the headers only.
.fcs_channel_table <- function(files) {
  do.call(rbind, lapply(files, function(f) {
    kw <- flowCore::read.FCSheader(f)[[1]]
    n <- as.integer(kw[["$PAR"]])
    get <- function(key) vapply(seq_len(n), function(i) {
      v <- kw[sprintf(key, i)]
      if (is.null(v) || is.na(v) || !nzchar(trimws(v))) NA_character_ else trimws(unname(v))
    }, character(1))
    channel <- get("$P%dN")
    marker <- get("$P%dS")
    data.frame(file = basename(f), channel = channel, marker = marker,
               # what identifies a channel across files: its marker if it has
               # one, otherwise its own name (scatter, time and the like)
               id = ifelse(is.na(marker) | marker == channel, channel, marker),
               stringsAsFactors = FALSE)
  }))
}

#' Work out how a set of FCS files line up
#'
#' Reads the header of every file and matches each file's channels to the
#' first file's: channels with a marker by the marker's name, the others
#' (scatter, time and the like) by the channel's name, ignoring capitals,
#' spaces, hyphens and underscores. Markers named in the metadata sheet take
#' the sheet's spelling.
#'
#' @param files Paths of the FCS files, the first being the reference.
#' @param sheet_markers The marker names typed in the metadata sheet.
#' @return A list with
#'   \describe{
#'     \item{reference}{a data frame of the lined-up channels, in order:
#'       `channel` (the first file's channel name) and `marker` (the sheet's
#'       spelling for markers in the sheet, otherwise the first file's).}
#'     \item{per_file}{for each file (by base name), a named character vector
#'       in reference order: the values are the file's own channel names, the
#'       names are the reference channel names they become.}
#'     \item{missing}{a data frame of sheet markers missing from some files:
#'       `marker` and `files` (comma-separated). Empty when nothing is missing.}
#'     \item{notes}{plain sentences describing what was lined up.}
#'   }
#' @export
plan_fcs_lineup <- function(files, sheet_markers = character()) {
  tab <- .fcs_channel_table(files)
  tab$key <- name_key(tab$id)
  file_names <- basename(files)
  notes <- character()

  # A file with two channels for the same marker: use the first, say so
  dup <- tab[duplicated(tab[c("file", "key")]) | duplicated(tab[c("file", "key")], fromLast = TRUE), ]
  if (nrow(dup)) {
    for (k in unique(dup$key)) {
      d <- dup[dup$key == k, ]
      notes <- c(notes, sprintf(
        "'%s' is on more than one channel in %d file(s) (%s); the first, %s, was used.",
        d$id[1], length(unique(d$file)), paste(unique(d$channel), collapse = ", "), d$channel[1]))
    }
    tab <- tab[!duplicated(tab[c("file", "key")]), ]
  }

  # Channels found in every file, in the first file's order
  ref <- tab[tab$file == file_names[1], ]
  in_all <- vapply(ref$key, function(k) all(file_names %in% tab$file[tab$key == k]), logical(1))

  # Sheet markers: each must be in every file
  sheet_markers <- unique(stats::na.omit(as.character(sheet_markers)))
  sheet_key <- name_key(sheet_markers)
  missing <- data.frame(marker = character(), files = character(), stringsAsFactors = FALSE)
  for (i in seq_along(sheet_markers)) {
    absent <- setdiff(file_names, tab$file[tab$key == sheet_key[i]])
    if (length(absent)) missing[nrow(missing) + 1, ] <- list(sheet_markers[i], paste(absent, collapse = ", "))
  }

  kept_keys <- ref$key[in_all]
  dropped <- unique(tab$id[!tab$key %in% kept_keys & !tab$key %in% sheet_key])
  if (length(dropped)) {
    notes <- c(notes, sprintf(
      "%d channel(s) not found in every file were left out (none is used by the analysis): %s.",
      length(dropped), paste(dropped, collapse = ", ")))
  }
  ref <- ref[in_all, ]

  # Spelling: the sheet's for sheet markers, otherwise the first file's
  ref$marker_out <- ifelse(is.na(ref$marker), NA_character_, ref$marker)
  at_sheet <- match(ref$key, sheet_key)
  ref$marker_out[!is.na(at_sheet)] <- sheet_markers[at_sheet[!is.na(at_sheet)]]

  per_file <- stats::setNames(lapply(file_names, function(fn) {
    ft <- tab[tab$file == fn, ]
    stats::setNames(ft$channel[match(ref$key, ft$key)], ref$channel)
  }), file_names)

  # Describe the sheet markers that were spelt differently in the files
  for (i in which(!is.na(at_sheet))) {
    k <- ref$key[i]; want <- ref$marker_out[i]
    spellings <- tab$id[tab$key == k & tab$file %in% file_names]
    other <- table(spellings[spellings != want])
    for (s in names(other)) {
      notes <- c(notes, sprintf("'%s' in %d file(s) was taken as '%s'.", s, other[[s]], want))
    }
  }
  # ... and the markers that sit on different channels in different files
  for (i in seq_len(nrow(ref))) {
    if (is.na(ref$marker[i])) next
    chans <- vapply(per_file, `[[`, character(1), ref$channel[i])
    by_chan <- table(chans)
    if (length(unique(name_key(names(by_chan)))) > 1) {
      notes <- c(notes, sprintf(
        "'%s' is on %s. Check its cofactor suits every file: the sheet holds one per marker.",
        ref$marker_out[i],
        paste(sprintf("%s in %d file(s)", names(by_chan), as.integer(by_chan)), collapse = " and ")))
    }
  }

  list(
    reference = data.frame(channel = ref$channel, marker = ref$marker_out, stringsAsFactors = FALSE),
    per_file = per_file,
    missing = missing,
    notes = notes
  )
}

#' Line one FCS file up with the others
#'
#' Keeps the file's lined-up channels in the reference order, gives them the
#' reference channel names and the agreed marker names. The cells and their
#' values are untouched.
#'
#' @param ff A flowFrame, read from the original file.
#' @param lineup The result of [plan_fcs_lineup()].
#' @param file The file's base name, as in `names(lineup$per_file)`.
#' @return The lined-up flowFrame.
#' @export
lineup_fcs_frame <- function(ff, lineup, file) {
  map <- lineup$per_file[[file]]
  if (is.null(map)) stop("No line-up found for ", file)
  markers <- lineup$reference$marker[match(names(map), lineup$reference$channel)]
  # Nothing to line up: leave the file exactly as it was read
  current <- flowCore::pData(flowCore::parameters(ff))
  cur_name <- unname(as.character(current$name))
  cur_desc <- unname(trimws(as.character(current$desc)))
  cur_desc[is.na(cur_desc) | cur_desc == cur_name] <- NA_character_
  want_desc <- unname(markers)
  want_desc[!is.na(want_desc) & want_desc == names(map)] <- NA_character_
  if (identical(cur_name, names(map)) && identical(unname(map), names(map)) &&
      identical(cur_desc, want_desc)) {
    return(ff)
  }
  ff <- ff[, unname(map)]
  flowCore::colnames(ff) <- names(map)
  has_marker <- !is.na(markers)
  if (any(has_marker)) {
    flowCore::markernames(ff) <- stats::setNames(markers[has_marker], names(map)[has_marker])
  }
  ff
}
