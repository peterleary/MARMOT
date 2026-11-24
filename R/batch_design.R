# Checking a batch design before any heavy work
#
# When the user names a batch column, the batch enters the DA/DS models and
# Harmony. Some designs cannot work: a batch that is the condition under
# another name, a single batch, a batch of one sample, too few samples. Left
# unchecked, DA stops with a message about matrix rank, and DS carries on
# without stopping and returns partly empty results. So the design is checked
# first, and a design that cannot work stops the run in plain words.

#' Check a batch design
#'
#' Stops, in plain words, when the batches cannot be separated from the
#' conditions or leave too little to estimate noise. Returns notes for the
#' report when the design works but some batches add nothing to a comparison.
#'
#' @param batch The batch of each sample.
#' @param condition The condition of each sample.
#' @param sample_id The sample IDs, for the messages.
#' @param column The name of the batch column, for the messages.
#' @return A character vector of notes (empty when there is nothing to say),
#'   invisibly. Stops with a message when the design cannot work.
#' @export
check_batch_design <- function(batch, condition, sample_id, column = "batch") {
  batch <- as.character(batch); condition <- as.character(condition)
  quote <- function(x) paste0("'", x, "'", collapse = ", ")

  blank <- is.na(batch) | !nzchar(trimws(batch))
  if (any(blank)) {
    stop("\u274c The batch column '", column, "' has no batch for ", sum(blank), " sample(s): ",
         paste(sample_id[blank], collapse = ", "),
         ". Every sample needs a batch, or leave the batchColumn setting blank.", call. = FALSE)
  }
  batches <- unique(batch); conds <- unique(condition)
  if (length(batches) < 2) {
    stop("\u274c Every sample is in the same batch (", quote(batches), "), so there is nothing to correct for. ",
         "Leave the batchColumn setting blank.", call. = FALSE)
  }
  per_batch <- table(batch)
  if (any(per_batch < 2)) {
    single <- names(per_batch)[per_batch < 2]
    stop("\u274c ", length(single), " batch(es) hold only one sample: ", quote(single), ". ",
         "A batch of one sample cannot be told apart from that sample itself, so it adds nothing to the ",
         "comparison. Merge it into another batch, or leave the batchColumn setting blank.", call. = FALSE)
  }
  conds_per_batch <- tapply(condition, batch, function(x) length(unique(x)))
  batches_per_cond <- tapply(batch, condition, function(x) length(unique(x)))
  if (all(conds_per_batch == 1) && all(batches_per_cond == 1)) {
    stop("\u274c Batch and condition are the same thing in this experiment: each batch holds only one condition. ",
         "The batch effect cannot be separated from the condition effect, so neither can be corrected for. ",
         "Leave the batchColumn setting blank.", call. = FALSE)
  }
  n <- length(sample_id); p <- length(conds) + length(batches) - 1
  if (n - p < 2) {
    stop("\u274c Too few samples (", n, ") for ", length(conds), " conditions and ", length(batches),
         " batches: fewer than two would be left to estimate the noise. ",
         "Leave the batchColumn setting blank, or add samples.", call. = FALSE)
  }
  design <- stats::model.matrix(~ 0 + factor(condition) + factor(batch))
  if (qr(design)$rank < ncol(design)) {
    stop("\u274c The batches and conditions overlap in a way that leaves some effects impossible to tell apart. ",
         "Check the batch column '", column, "', or leave the batchColumn setting blank.", call. = FALSE)
  }

  notes <- character()
  one_cond <- names(conds_per_batch)[conds_per_batch == 1]
  if (length(one_cond)) {
    notes <- c(notes, sprintf(
      "%d batch(es) hold only one condition (%s), so they add little to comparing conditions; the other batches carry those comparisons.",
      length(one_cond), quote(one_cond)))
  }
  invisible(notes)
}
