# =============================================================================
# Mpacmap: R port of PaCMAP v0.8.0
# =============================================================================
# Faithful port of pacmap.pacmap.pacmap() optimization loop
# Dependencies: RcppHNSW, Matrix
# =============================================================================

#' PaCMAP dimensionality reduction (pure R implementation)
#'
#' @param data Numeric matrix (cells x features)
#' @param n_components Output dimensions (default 2)
#' @param n_neighbors Number of nearest-neighbor pairs per point (default 10)
#' @param MN_ratio Mid-near pair ratio (default 0.5)
#' @param FP_ratio Further pair ratio (default 2.0)
#' @param num_iters Iteration counts per phase: c(phase1, phase2, phase3)
#' @param lr Adam learning rate (default 1.0)
#' @param apply_pca Apply PCA to 100D if ncol > 100 (default TRUE)
#' @param init Initialization: "pca" or "random"
#' @param verbose Print progress
#' @param seed Random seed (NULL for non-deterministic)
#' @return n x n_components numeric matrix of embedding coordinates
#' @export
Mpacmap <- function(data,
                    n_components = 2L,
                    n_neighbors = 10L,
                    MN_ratio = 0.5,
                    FP_ratio = 2.0,
                    num_iters = c(100L, 100L, 250L),
                    lr = 1.0,
                    apply_pca = TRUE,
                    init = "pca",
                    verbose = FALSE,
                    seed = NULL) {

  if (!requireNamespace("RcppHNSW", quietly = TRUE))
    stop("RcppHNSW is required for Mpacmap()")
  if (!requireNamespace("Matrix", quietly = TRUE))
    stop("Matrix is required for Mpacmap()")

  data <- as.matrix(data)
  n <- nrow(data)
  d <- ncol(data)
  n_components <- as.integer(n_components)

  if (!is.null(seed)) set.seed(seed)

  if (verbose) message("Mpacmap: n=", n, " d=", d)

  # ---- Step 1: Preprocessing ----
  prep <- .pacmap_preprocess(data, apply_pca, n_components, seed, verbose)
  X <- prep$X
  pca_solution <- prep$pca_solution
  pca_result <- prep$pca_result

  # ---- Step 2: k-NN + sigma + pair selection ----
  n_neighbors <- min(n_neighbors, n - 1L)
  n_MN <- as.integer(round(n_neighbors * MN_ratio))
  n_FP <- as.integer(round(n_neighbors * FP_ratio))
  n_MN <- min(n_MN, n - 1L)
  n_FP <- min(n_FP, n - 1L)

  if (verbose) message("Pair counts: NN=", n_neighbors, " MN=", n_MN, " FP=", n_FP)

  pairs <- .pacmap_generate_pairs(X, n, n_neighbors, n_MN, n_FP, seed, verbose)

  # ---- Step 3: Initialization ----
  Y <- .pacmap_init(X, n, n_components, init, pca_solution, pca_result, seed)

  # ---- Step 4: Three-phase Adam optimization ----
  Y <- .pacmap_optimize(Y, pairs$nb, pairs$mn, pairs$fp, n, n_components,
                        num_iters, lr, verbose)

  Y
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Preprocess: PCA if high-dim, else min-max normalize + center
#' @keywords internal
.pacmap_preprocess <- function(X, apply_pca, n_components, seed, verbose) {
  n <- nrow(X)
  d <- ncol(X)
  pca_result <- NULL
  pca_solution <- FALSE

  if (d > 100 && apply_pca) {
    # Truncated SVD to 100D (center first)
    if (verbose) message("Applying PCA: ", d, "D -> 100D")
    X_mean <- colMeans(X)
    X <- sweep(X, 2, X_mean)
    pca_result <- prcomp(X, center = FALSE, rank. = min(100, n - 1, d))
    X <- pca_result$x[, seq_len(min(100, ncol(pca_result$x))), drop = FALSE]
    pca_solution <- TRUE
  } else {
    # Normalize then center (matches Python: X -= xmin; X /= xmax; X -= mean)
    xmin <- min(X)
    xmax <- max(X)
    X <- X - xmin
    if (xmax != 0) X <- X / xmax
    X_mean <- colMeans(X)
    X <- sweep(X, 2, X_mean)
    # Still compute PCA for initialization
    pca_result <- prcomp(X, center = FALSE,
                         rank. = min(n_components, n - 1, ncol(X)))
  }

  list(X = X, pca_solution = pca_solution, pca_result = pca_result)
}


#' Generate all three pair types
#' @keywords internal
.pacmap_generate_pairs <- function(X, n, n_neighbors, n_MN, n_FP, seed, verbose) {
  d <- ncol(X)

  # Extra neighbors for robust sigma estimation
  n_neighbors_extra <- min(n_neighbors + 50L, n - 1L)

  if (verbose) message("Building HNSW index (k=", n_neighbors_extra, ")")

  # HNSW k-NN search
  knn <- RcppHNSW::hnsw_knn(X, k = n_neighbors_extra + 1L,
                              distance = "euclidean")

  # Drop self (first column)
  nbrs <- knn$idx[, -1, drop = FALSE]
  knn_dist <- knn$dist[, -1, drop = FALSE]

  # Trim to n_neighbors_extra
  if (ncol(nbrs) > n_neighbors_extra) {
    nbrs <- nbrs[, seq_len(n_neighbors_extra), drop = FALSE]
    knn_dist <- knn_dist[, seq_len(n_neighbors_extra), drop = FALSE]
  }

  # ---- Sigma: mean of distances to 4th, 5th, 6th neighbors ----
  # Python uses knn_distances[:, 3:6] (0-indexed columns 3,4,5)
  sig_cols <- seq(min(4, ncol(knn_dist)), min(6, ncol(knn_dist)))
  sig <- pmax(rowMeans(knn_dist[, sig_cols, drop = FALSE]), 1e-10)

  if (verbose) message("Computed sigma")

  # ---- Scaled distance ----
  # scaled_dist[i,j] = knn_dist[i,j]^2 / (sig[i] * sig[nbrs[i,j]])
  scaled_dist <- matrix(0, n, ncol(nbrs))
  for (j in seq_len(ncol(nbrs))) {
    scaled_dist[, j] <- knn_dist[, j]^2 / (sig * sig[nbrs[, j]])
  }

  if (verbose) message("Computed scaled distances")

  # ---- Nearest neighbor pairs: sort by scaled distance, keep top n_neighbors ----
  pair_nb <- .pacmap_sample_neighbors(scaled_dist, nbrs, n, n_neighbors)

  if (verbose) message("Sampled ", nrow(pair_nb), " neighbor pairs")

  # ---- Mid-near pairs ----
  pair_mn <- .pacmap_sample_MN(X, n, n_MN, seed)

  if (verbose) message("Sampled ", nrow(pair_mn), " mid-near pairs")

  # ---- Further pairs ----
  pair_fp <- .pacmap_sample_FP(n, n_FP, nbrs, n_neighbors, seed)

  if (verbose) message("Sampled ", nrow(pair_fp), " further pairs")

  list(nb = pair_nb, mn = pair_mn, fp = pair_fp)
}


#' Sample nearest neighbor pairs (sort by scaled distance, keep top n_neighbors)
#' @keywords internal
.pacmap_sample_neighbors <- function(scaled_dist, nbrs, n, n_neighbors) {
  # Vectorized: sort each row, take top n_neighbors
  sorted_orders <- t(apply(scaled_dist, 1, order))
  # Take only the first n_neighbors columns
  top_k <- sorted_orders[, seq_len(n_neighbors), drop = FALSE]
  # Look up actual neighbor indices
  pair_j <- matrix(0L, n, n_neighbors)
  for (j in seq_len(n_neighbors)) {
    pair_j[, j] <- nbrs[cbind(seq_len(n), top_k[, j])]
  }
  pairs <- cbind(
    rep(seq_len(n), n_neighbors),
    as.vector(pair_j)
  )
  storage.mode(pairs) <- "integer"
  pairs
}


#' Sample mid-near pairs (6 random candidates, discard closest, pick 2nd closest)
#' @keywords internal
.pacmap_sample_MN <- function(X, n, n_MN, seed) {
  if (n_MN == 0) return(matrix(0L, 0, 2))

  pairs <- matrix(0L, n * n_MN, 2)
  idx <- 0L
  for (i in seq_len(n)) {
    already_picked <- integer(0)
    for (j in seq_len(n_MN)) {
      # Sample 6 random non-self, non-already-picked candidates
      candidates <- .sample_reject(6L, n, exclude = c(i, already_picked))
      # Compute Euclidean distances
      dists <- sqrt(rowSums(sweep(X[candidates, , drop = FALSE], 2, X[i, ])^2))
      # Discard closest, pick second closest
      sorted_idx <- order(dists)
      # Remove the closest (index 1), pick from remaining the closest
      picked <- candidates[sorted_idx[2]]
      already_picked <- c(already_picked, picked)
      idx <- idx + 1L
      pairs[idx, 1] <- i
      pairs[idx, 2] <- picked
    }
  }
  pairs
}


#' Sample further pairs with rejection sampling
#' @keywords internal
.pacmap_sample_FP <- function(n, n_FP, nbrs, n_neighbors, seed) {
  if (n_FP == 0) return(matrix(0L, 0, 2))

  pairs <- matrix(0L, n * n_FP, 2)
  idx <- 0L
  # Get the actual nearest neighbor indices per point (first n_neighbors columns)
  nn_cols <- min(n_neighbors, ncol(nbrs))

  for (i in seq_len(n)) {
    nn_set <- nbrs[i, seq_len(nn_cols)]
    already_picked <- integer(0)
    for (j in seq_len(n_FP)) {
      exclude <- c(i, nn_set, already_picked)
      fp_idx <- .sample_reject(1L, n, exclude = exclude, max_tries = 100L)
      already_picked <- c(already_picked, fp_idx)
      idx <- idx + 1L
      pairs[idx, 1] <- i
      pairs[idx, 2] <- fp_idx
    }
  }
  pairs
}


#' Rejection sampler: pick n_samples from 1:maximum, excluding 'exclude'
#' @keywords internal
.sample_reject <- function(n_samples, maximum, exclude, max_tries = 1000L) {
  result <- integer(n_samples)
  filled <- 0L
  tries <- 0L
  while (filled < n_samples && tries < max_tries) {
    j <- sample.int(maximum, 1L)
    tries <- tries + 1L
    if (j %in% exclude || j %in% result[seq_len(filled)]) next
    filled <- filled + 1L
    result[filled] <- j
  }
  # If we couldn't fill all, sample without constraints for remaining
  if (filled < n_samples) {
    remaining <- setdiff(seq_len(maximum), c(exclude, result[seq_len(filled)]))
    need <- n_samples - filled
    if (length(remaining) >= need) {
      result[(filled + 1L):n_samples] <- sample(remaining, need)
    } else if (length(remaining) > 0) {
      result[(filled + 1L):(filled + length(remaining))] <- remaining
      # Fill rest with random (allow collisions as last resort)
      if (filled + length(remaining) < n_samples) {
        rest <- (filled + length(remaining) + 1L):n_samples
        result[rest] <- sample.int(maximum, length(rest))
      }
    }
  }
  result
}


#' Initialize embedding
#' @keywords internal
.pacmap_init <- function(X, n, n_components, init, pca_solution,
                         pca_result, seed) {
  nc <- min(n_components, ncol(pca_result$x))

  if (init == "pca") {
    if (pca_solution) {
      # X is already PCA-transformed; use first n_components columns
      Y <- 0.01 * X[, seq_len(nc), drop = FALSE]
    } else {
      # Use pre-computed PCA
      Y <- 0.01 * pca_result$x[, seq_len(nc), drop = FALSE]
    }
  } else {
    # Random initialization
    if (!is.null(seed)) set.seed(seed)
    Y <- matrix(rnorm(n * n_components), n, n_components) * 0.0001
  }

  storage.mode(Y) <- "double"
  Y
}


#' Three-phase Adam optimization (vectorized via sparse incidence matrices)
#' @keywords internal
.pacmap_optimize <- function(Y, pair_nb, pair_mn, pair_fp,
                             n, n_components, num_iters, lr, verbose) {
  # Adam parameters
  beta1 <- 0.9
  beta2 <- 0.999
  eps <- 1e-7
  w_MN_init <- 1000.0

  m <- matrix(0, n, n_components)
  v <- matrix(0, n, n_components)

  # Pre-build sparse incidence matrices for gradient accumulation
  # S %*% g maps pair-level gradients to node-level gradients
  n_nb <- nrow(pair_nb)
  n_mn <- nrow(pair_mn)
  n_fp <- nrow(pair_fp)

  S_nb <- .build_incidence(pair_nb, n)
  S_mn <- if (n_mn > 0) .build_incidence(pair_mn, n) else NULL
  S_fp <- if (n_fp > 0) .build_incidence(pair_fp, n) else NULL

  total_iters <- sum(num_iters)
  phase1 <- num_iters[1]
  phase2 <- num_iters[1] + num_iters[2]

  if (verbose) message("Starting optimization (", total_iters, " iterations)")

  for (itr in seq_len(total_iters)) {
    itr0 <- itr - 1L  # 0-indexed for weight schedule

    # ---- Weight schedule ----
    if (itr0 < phase1) {
      t_frac <- itr0 / phase1
      w_nb <- 2.0
      w_mn <- (1 - t_frac) * w_MN_init + t_frac * 3.0
      w_fp <- 1.0
    } else if (itr0 < phase2) {
      w_nb <- 3.0
      w_mn <- 3.0
      w_fp <- 1.0
    } else {
      w_nb <- 1.0
      w_mn <- 0.0
      w_fp <- 1.0
    }

    # ---- Compute gradients (vectorized) ----
    grad <- matrix(0, n, n_components)

    # Neighbor gradient (attractive)
    if (n_nb > 0) {
      diff_nb <- Y[pair_nb[, 1], , drop = FALSE] - Y[pair_nb[, 2], , drop = FALSE]
      d_nb <- 1 + rowSums(diff_nb^2)
      w1_nb <- w_nb * 20.0 / (10.0 + d_nb)^2
      g_nb <- w1_nb * diff_nb
      grad <- grad + as.matrix(S_nb %*% g_nb)
    }

    # Mid-near gradient (attractive)
    if (n_mn > 0 && w_mn > 0) {
      diff_mn <- Y[pair_mn[, 1], , drop = FALSE] - Y[pair_mn[, 2], , drop = FALSE]
      d_mn <- 1 + rowSums(diff_mn^2)
      w1_mn <- w_mn * 20000.0 / (10000.0 + d_mn)^2
      g_mn <- w1_mn * diff_mn
      grad <- grad + as.matrix(S_mn %*% g_mn)
    }

    # Far gradient (repulsive — note sign flip)
    if (n_fp > 0) {
      diff_fp <- Y[pair_fp[, 1], , drop = FALSE] - Y[pair_fp[, 2], , drop = FALSE]
      d_fp <- 1 + rowSums(diff_fp^2)
      w1_fp <- w_fp * 2.0 / (1.0 + d_fp)^2
      g_fp <- w1_fp * diff_fp
      grad <- grad - as.matrix(S_fp %*% g_fp)  # repulsive: subtract
    }

    # ---- Adam update ----
    # Bias-corrected learning rate
    lr_t <- lr * sqrt(1 - beta2^itr) / (1 - beta1^itr)

    m <- m + (1 - beta1) * (grad - m)
    v <- v + (1 - beta2) * (grad^2 - v)
    Y <- Y - lr_t * m / (sqrt(v) + eps)

    if (verbose && itr %% 10 == 0) {
      # Approximate loss for monitoring
      loss <- 0
      if (n_nb > 0) {
        loss <- loss + sum(w_nb * d_nb / (10 + d_nb))
      }
      message("  Iteration ", itr, "/", total_iters, " loss=", round(loss, 2))
    }
  }

  if (verbose) message("Optimization complete")
  Y
}


#' Build sparse incidence matrix for a set of pairs
#' @keywords internal
.build_incidence <- function(pairs, n) {
  n_pairs <- nrow(pairs)
  # Positive entries for source nodes
  # Negative entries for target nodes
  i_idx <- c(pairs[, 1], pairs[, 2])
  j_idx <- rep(seq_len(n_pairs), 2)
  x_val <- c(rep(1, n_pairs), rep(-1, n_pairs))

  Matrix::sparseMatrix(i = i_idx, j = j_idx, x = x_val,
                       dims = c(n, n_pairs))
}
