#' How well batches mix
#'
#' For a sample of cells, the share of each cell's nearest neighbours that
#' come from another batch, divided by the share expected if the batches were
#' thoroughly mixed. 1 means the batches are fully mixed; near 0, they sit
#' apart. Used by the report's Batch correction tab to compare before and
#' after correction. The random-number stream is left as it was found.
#'
#' @param coords A cells x dimensions matrix (for example PCA positions).
#' @param batch The batch of each cell.
#' @param k Neighbours per cell.
#' @param n Cells to sample (all of them if there are fewer).
#' @param seed Seed for the sample, so the score repeats exactly.
#' @return A single number.
#' @export
batch_mixing <- function(coords, batch, k = 30, n = 20000, seed = 2) {
  coords <- as.matrix(coords); batch <- as.character(batch)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
          else rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  pick <- if (nrow(coords) > n) sort(sample.int(nrow(coords), n)) else seq_len(nrow(coords))
  k <- min(k, nrow(coords) - 1)
  nn <- RANN::nn2(coords, coords[pick, , drop = FALSE], k = k + 1)$nn.idx[, -1, drop = FALSE]
  other <- rowMeans(matrix(batch[nn], ncol = k) != batch[pick])
  share <- table(batch) / length(batch)
  expected <- 1 - as.numeric(share[batch[pick]])
  mean(other / expected)
}
