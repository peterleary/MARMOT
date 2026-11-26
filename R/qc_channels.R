# Channels that quality control should not judge
#
# Some instruments write channels that are not biology: imaging features, sort
# positions, saturation flags and the like. Quality control (FlowAI, PeacoQC)
# looks at nearly every channel, so these channels can make good cells look bad.
# Two things decide which channels are kept out of QC:
#
# 1. The table of channels that are never markers, per instrument, in
#    inst/extdata/instrument_channels.json, matched on the instrument name
#    each FCS file carries ($CYT). QC skips the kinds it lists in qc_skips.
# 2. A check for any instrument: FCS 3.2 lets a channel be stored differently
#    from the rest of the file ($PnDATATYPE). flowCore ignores that and reads
#    such channels as garbage, so they are always kept out of QC.
#
# The channels stay in the data. Only QC stops looking at them.

#' The table of channels to keep out of QC, per instrument
#'
#' Read from `inst/extdata/instrument_channels.json`: the channels of the
#' kinds QC skips, named exactly (not the `#` patterns).
#'
#' @return A data frame with columns `instrument`, `channel` and `reason`.
#' @export
qc_ignore_table <- function() {
  tab <- jsonlite::read_json(system.file("extdata", "instrument_channels.json", package = "MARMOT"))
  rows <- lapply(names(tab$instruments), function(inst) {
    ch <- tab$instruments[[inst]]
    data.frame(instrument = rep(inst, length(ch)), channel = names(ch),
               reason = vapply(ch, as.character, character(1), USE.NAMES = FALSE),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[out$reason %in% unlist(tab$qc_skips) & !grepl("#", out$channel, fixed = TRUE) & out$instrument != "*", , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Find the channels quality control should not judge
#'
#' Reads the headers of the FCS files only. A channel is listed if the table
#' names it for the file's instrument, or if the file stores it in a different
#' data type from the rest (which flowCore cannot read correctly).
#'
#' @param files Paths of the FCS files.
#' @param ignore_table The per-instrument table; defaults to [qc_ignore_table()].
#' @return A data frame with one row per channel: `channel`, `reason` and
#'   `files` (how many of the files have it).
#' @export
qc_ignore_channels <- function(files, ignore_table = qc_ignore_table()) {
  rows <- lapply(files, function(f) {
    kw <- flowCore::read.FCSheader(f)[[1]]
    n <- as.integer(kw[["$PAR"]])
    channel <- vapply(seq_len(n), function(i) trimws(unname(kw[sprintf("$P%dN", i)])), character(1))
    instrument <- trimws(unname(kw["$CYT"]))
    hits <- if (is.na(instrument)) ignore_table[0, ] else
      ignore_table[tolower(ignore_table$instrument) == tolower(instrument) & ignore_table$channel %in% channel, ]
    out <- data.frame(channel = hits$channel, reason = hits$reason, stringsAsFactors = FALSE)
    file_type <- unname(kw["$DATATYPE"])
    own_type <- vapply(seq_len(n), function(i) unname(kw[sprintf("$P%dDATATYPE", i)]), character(1))
    odd <- !is.na(own_type) & own_type != file_type
    if (any(odd)) {
      out <- rbind(out, data.frame(channel = channel[odd],
                                   reason = "stored in a format flowCore cannot read",
                                   stringsAsFactors = FALSE))
    }
    out[!duplicated(out$channel, fromLast = TRUE), , drop = FALSE]
  })
  all <- do.call(rbind, rows)
  if (is.null(all) || nrow(all) == 0) {
    return(data.frame(channel = character(), reason = character(), files = integer()))
  }
  counts <- table(all$channel)
  all <- all[!duplicated(all$channel, fromLast = TRUE), , drop = FALSE]
  all$files <- as.integer(counts[all$channel])
  rownames(all) <- NULL
  all
}

#' Turn channel names into exact-match patterns
#'
#' FlowAI joins its exclusion list into one regular expression, so channel
#' names with brackets or dots must be escaped and anchored.
#'
#' @param channels Channel names.
#' @return One pattern per channel, matching that name exactly.
#' @export
exact_channel_patterns <- function(channels) {
  if (length(channels) == 0) return(character())
  paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", channels), "$")
}
