# =============================================================================
# Mparc: R port of PARC v0.40 clustering
# =============================================================================
# Faithful port of parc._parc.PARC.run_subPARC()
# Dependencies: RcppHNSW, igraph, Matrix
# =============================================================================

#' PARC clustering (pure R implementation)
#'
#' @param data Numeric matrix (cells x features)
#' @param k Number of nearest neighbors (default 30)
#' @param dist_std_local Local pruning threshold in SDs above mean (default 3)
#' @param jac_std_global "median" or numeric SDs below mean for Jaccard threshold
#' @param keep_all_local_dist "auto", TRUE, or FALSE — skip local pruning
#' @param too_big_factor Fraction of n above which clusters get sub-clustered
#' @param small_pop Minimum cluster size; smaller clusters get merged
#' @param jac_weighted_edges Use Jaccard weights in Leiden (default TRUE)
#' @param n_iter_leiden Number of Leiden iterations (default 5)
#' @param seed Random seed
#' @param num_threads Threads for HNSW (default 1)
#' @param distance Distance metric for HNSW ("l2", "cosine", "ip")
#' @param time_smallpop Max seconds for small-pop merging loop
#' @param partition_type "ModularityVP" or "RBVP"
#' @param resolution_parameter Leiden resolution (triggers RBVP if != 1)
#' @param hnsw_param_ef_construction HNSW ef_construction (default 150)
#' @param verbose Print progress messages
#' @return Integer vector of 1-indexed cluster labels (length = nrow(data))
#' @export
Mparc <- function(data,
                  k = 30L,
                  dist_std_local = 3,
                  jac_std_global = "median",
                  keep_all_local_dist = "auto",
                  too_big_factor = 0.4,
                  small_pop = 10L,
                  jac_weighted_edges = TRUE,
                  n_iter_leiden = 5L,
                  seed = 42L,
                  num_threads = 1L,
                  distance = "l2",
                  time_smallpop = 15,
                  partition_type = "ModularityVP",
                  resolution_parameter = 1.0,
                  hnsw_param_ef_construction = 150L,
                  verbose = FALSE) {

  if (!requireNamespace("RcppHNSW", quietly = TRUE))
    stop("RcppHNSW is required for Mparc()")
  if (!requireNamespace("igraph", quietly = TRUE))
    stop("igraph is required for Mparc()")
  if (!requireNamespace("Matrix", quietly = TRUE))
    stop("Matrix is required for Mparc()")

  data <- as.matrix(data)
  n <- nrow(data)
  d <- ncol(data)
  k <- as.integer(k)

  # Resolve keep_all_local_dist
  if (identical(keep_all_local_dist, "auto")) {
    keep_all_local_dist <- n > 300000
  }

  # Resolve partition type (Python: if resolution != 1, switch to RBVP)
  if (resolution_parameter != 1.0) {
    partition_type <- "RBVP"
  }

  if (verbose) message("Mparc: n=", n, " d=", d, " k=", k)

  # ---- Step 1: HNSW k-NN ----
  knn_result <- .parc_build_knn(data, k, n, d, distance,
                                hnsw_param_ef_construction, num_threads)
  neighbor_array <- knn_result$idx   # n x k (1-indexed)
  distance_array <- knn_result$dist  # n x k

  # ---- Step 2: Build CSR-like adjacency (local pruning) ----
  csr <- .parc_make_csr(neighbor_array, distance_array, n, k,
                        keep_all_local_dist, dist_std_local, verbose)

  # ---- Step 3-4: Jaccard similarity + global threshold ----
  pruned <- .parc_jaccard_prune(csr, n, jac_std_global, verbose)

  # ---- Step 5-6: Build graph + Leiden ----
  labels <- .parc_leiden(pruned, n, jac_weighted_edges, partition_type,
                         resolution_parameter, n_iter_leiden, seed, verbose)

  # ---- Step 7: Too-big splitting ----
  labels <- .parc_split_toobig(labels, data, neighbor_array, distance_array,
                               n, k, too_big_factor, keep_all_local_dist,
                               dist_std_local, jac_std_global,
                               jac_weighted_edges, partition_type,
                               resolution_parameter, n_iter_leiden, seed,
                               small_pop, time_smallpop, distance,
                               hnsw_param_ef_construction, num_threads,
                               verbose)

  # ---- Step 8: Small-pop merging ----
  labels <- .parc_merge_small(labels, neighbor_array, small_pop,
                              time_smallpop, verbose)

  # Final contiguous relabeling from 1
  as.integer(as.integer(factor(labels)))
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Build HNSW index and query k-NN
#' @keywords internal
.parc_build_knn <- function(data, k, n, d, distance,
                            ef_construction, num_threads) {
  # Map distance names: PARC uses hnswlib names
  dist_map <- c(l2 = "euclidean", cosine = "cosine", ip = "euclidean")
  hnsw_dist <- unname(dist_map[distance])
  if (is.na(hnsw_dist)) hnsw_dist <- "euclidean"

  # M parameter (Python: 48 if d>30 AND n<=50000, else 24)
  M <- if (d > 30 && n <= 50000) 48L else 24L

  # ef_construction: for small datasets, use min(n-10, 500)
  if (n < 10000) {
    ef_construction <- min(n - 10L, 500L)
  }

  # ef_query
  ef_query <- if (n < 10000) ef_construction else max(100L, k + 1L)

  # Query k+1 neighbors (includes self), then drop self
  k_query <- min(k + 1L, n - 1L)

  # RcppHNSW::hnsw_knn returns list(idx, dist) — 1-indexed, includes self
  result <- RcppHNSW::hnsw_knn(data, k = k_query,
                                distance = hnsw_dist,
                                M = M,
                                ef_construction = ef_construction,
                                ef = ef_query,
                                n_threads = num_threads)

  idx  <- result$idx
  dist <- result$dist

  # Drop self-neighbor (first column is self with dist ~0)
  # RcppHNSW returns self as first column
  if (ncol(idx) > k) {
    idx  <- idx[, -1, drop = FALSE]
    dist <- dist[, -1, drop = FALSE]
  }

  # Ensure we have exactly k neighbors (trim if needed)
  if (ncol(idx) > k) {
    idx  <- idx[, 1:k, drop = FALSE]
    dist <- dist[, 1:k, drop = FALSE]
  }

  list(idx = idx, dist = dist)
}


#' Build sparse adjacency from kNN with optional local pruning
#' @keywords internal
.parc_make_csr <- function(neighbor_array, distance_array, n, k,
                           keep_all_local_dist, dist_std_local, verbose) {
  if (!keep_all_local_dist) {
    # Local pruning: per row, keep neighbors where
    # dist < mean(row_dists) + dist_std_local * sd(row_dists)
    if (verbose) message("Local pruning at ", dist_std_local, " SD above mean")

    # Add 0.1 to distances (Python: distance_array = distance_array + 0.1)
    distance_array <- distance_array + 0.1

    rows <- cols <- weights <- vector("list", n)

    for (i in seq_len(n)) {
      dists <- distance_array[i, ]
      threshold <- mean(dists) + dist_std_local * sd(dists)
      keep <- which(dists < threshold)

      nbrs <- neighbor_array[i, keep]
      wts  <- dists[keep]

      # Remove self-loops (shouldn't happen after drop-self, but be safe)
      not_self <- nbrs != i
      nbrs <- nbrs[not_self]
      wts  <- wts[not_self]

      if (length(nbrs) > 0) {
        rows[[i]]    <- rep(i, length(nbrs))
        cols[[i]]    <- nbrs
        # Weight: 1 / (sqrt(d) + 0.1) for pruned path
        weights[[i]] <- 1.0 / (sqrt(wts) + 0.1)
      }
    }

    row_idx <- unlist(rows)
    col_idx <- unlist(cols)
    wt_vals <- unlist(weights)

  } else {
    # No local pruning: keep all edges
    # Weight: 1 / (d + 0.1)
    row_idx <- rep(seq_len(n), each = k)
    col_idx <- as.vector(t(neighbor_array))
    wt_vals <- 1.0 / (as.vector(t(distance_array)) + 0.1)

    # Remove self-loops
    not_self <- row_idx != col_idx
    row_idx <- row_idx[not_self]
    col_idx <- col_idx[not_self]
    wt_vals <- wt_vals[not_self]
  }

  # Build sparse matrix (1-indexed)
  Matrix::sparseMatrix(i = row_idx, j = col_idx, x = wt_vals,
                       dims = c(n, n))
}


#' Compute Jaccard similarity on edges and apply global threshold
#' @keywords internal
.parc_jaccard_prune <- function(csr, n, jac_std_global, verbose) {

  if (verbose) message("Computing Jaccard similarity")

  # Build igraph from the sparse adjacency
  # Extract edge list from sparse matrix
  csr_t <- Matrix::summary(csr)  # i, j, x triplets
  edge_i <- csr_t$i
  edge_j <- csr_t$j

  # Build igraph (1-indexed in R)
  g <- igraph::graph_from_edgelist(cbind(edge_i, edge_j), directed = TRUE)
  igraph::E(g)$weight <- csr_t$x

  # Compute Jaccard similarity for each edge
  # igraph::similarity.jaccard computes full n x n — too expensive.
  # Instead, compute per-edge Jaccard using adjacency sets.
  # For efficiency: binary adjacency → tcrossprod gives intersection counts.

  # Binary adjacency (unweighted)
  adj_bin <- csr > 0
  # Also make it include reverse edges for undirected Jaccard
  adj_sym <- adj_bin | Matrix::t(adj_bin)

  # Degree of each node (number of neighbors)
  degrees <- Matrix::rowSums(adj_sym)

  # Intersection matrix: B = adj_sym %*% t(adj_sym)
  # B[i,j] = number of common neighbors
  # Only compute for edges that exist
  B <- Matrix::tcrossprod(adj_sym)

  # Compute Jaccard for each edge (vectorized sparse matrix indexing)
  n_edges <- length(edge_i)
  intersections <- B[cbind(edge_i, edge_j)]
  union_sizes <- degrees[edge_i] + degrees[edge_j] - intersections
  jaccards <- ifelse(union_sizes > 0, intersections / union_sizes, 0)

  # Global threshold
  if (identical(jac_std_global, "median")) {
    threshold <- median(jaccards)
  } else {
    jac_std <- as.numeric(jac_std_global)
    threshold <- mean(jaccards) - jac_std * sd(jaccards)
  }

  if (verbose) message("Jaccard threshold: ", round(threshold, 4))

  # Keep edges above threshold
  keep <- jaccards > threshold
  if (verbose) {
    message("Edges kept after global pruning: ",
            sum(keep), "/", n_edges,
            " (", round(100 * sum(keep) / n_edges, 1), "%)")
  }

  list(
    edge_i = edge_i[keep],
    edge_j = edge_j[keep],
    jaccard = jaccards[keep],
    n = n
  )
}


#' Run Leiden community detection on the pruned Jaccard graph
#' @keywords internal
.parc_leiden <- function(pruned, n, jac_weighted_edges, partition_type,
                         resolution_parameter, n_iter_leiden, seed, verbose) {
  if (verbose) message("Running Leiden community detection")

  # Build graph
  el <- cbind(pruned$edge_i, pruned$edge_j)
  g <- igraph::make_empty_graph(n = n, directed = FALSE)
  g <- igraph::add_edges(g, t(el))

  if (jac_weighted_edges) {
    igraph::E(g)$weight <- pruned$jaccard
  }

  g <- igraph::simplify(g, edge.attr.comb = list(weight = "sum"))

  # Leiden clustering
  set.seed(seed)

  if (partition_type == "ModularityVP") {
    # Modularity-based (default)
    partition <- igraph::cluster_leiden(
      g,
      objective_function = "modularity",
      weights = if (jac_weighted_edges) igraph::E(g)$weight else NULL,
      n_iterations = n_iter_leiden
    )
  } else {
    # CPM / RBConfigurationVertexPartition
    partition <- igraph::cluster_leiden(
      g,
      objective_function = "CPM",
      resolution = resolution_parameter,
      weights = if (jac_weighted_edges) igraph::E(g)$weight else NULL,
      n_iterations = n_iter_leiden
    )
  }

  labels <- igraph::membership(partition)

  if (verbose) {
    n_clust <- length(unique(labels))
    message("Leiden found ", n_clust, " clusters")
  }

  labels
}


#' Split clusters that are too big by recursive sub-clustering
#' @keywords internal
.parc_split_toobig <- function(labels, data, neighbor_array, distance_array,
                               n, k, too_big_factor, keep_all_local_dist,
                               dist_std_local, jac_std_global,
                               jac_weighted_edges, partition_type,
                               resolution_parameter, n_iter_leiden, seed,
                               small_pop, time_smallpop, distance,
                               hnsw_param_ef_construction, num_threads,
                               verbose) {

  max_pop <- too_big_factor * n
  list_pop_too_bigs <- integer(0)

  repeat {
    tab <- table(labels)
    biggest_pop <- max(tab)
    if (biggest_pop <= max_pop) break

    biggest_label <- as.integer(names(tab)[which.max(tab)])

    # Check if we already tried to split this population size
    if (biggest_pop %in% list_pop_too_bigs) break
    list_pop_too_bigs <- c(list_pop_too_bigs, biggest_pop)

    cluster_locs <- which(labels == biggest_label)

    if (verbose) {
      message("Cluster ", biggest_label, " has ", length(cluster_locs),
              " cells (> ", round(max_pop), ") — sub-clustering")
    }

    # Sub-cluster the big cluster with tighter Jaccard threshold
    sub_data <- data[cluster_locs, , drop = FALSE]
    sub_n <- nrow(sub_data)

    sub_k <- if (sub_n > k) k else as.integer(max(5, 0.2 * sub_n))

    # Build sub-kNN
    sub_knn <- .parc_build_knn(sub_data, sub_k, sub_n, ncol(data), distance,
                               200L, num_threads)

    # Build sub-CSR
    sub_csr <- .parc_make_csr(sub_knn$idx, sub_knn$dist, sub_n, sub_k,
                              keep_all_local_dist, dist_std_local, verbose)

    # Jaccard prune with tighter threshold (jac_std_global = 0.3)
    sub_pruned <- .parc_jaccard_prune(sub_csr, sub_n, 0.3, verbose)

    # Leiden
    sub_labels <- .parc_leiden(sub_pruned, sub_n, jac_weighted_edges,
                               partition_type, resolution_parameter,
                               n_iter_leiden, seed, verbose)

    # Merge small clusters in the sub-result
    sub_labels <- .parc_merge_small(sub_labels, sub_knn$idx,
                                    small_pop, time_smallpop, FALSE)

    # Offset sub-labels to avoid collision
    sub_labels <- sub_labels + 100000L

    # Assign back
    labels[cluster_locs] <- sub_labels

    # Relabel contiguously
    labels <- as.integer(factor(labels))
  }

  labels
}


#' Merge clusters smaller than small_pop into nearest neighbor clusters
#' @keywords internal
.parc_merge_small <- function(labels, neighbor_array, small_pop,
                              time_smallpop, verbose) {

  # First pass: merge into most frequent non-small neighbor
  tab <- table(labels)
  small_clusters <- as.integer(names(tab)[tab < small_pop])

  if (length(small_clusters) > 0 && verbose) {
    message("Merging ", length(small_clusters), " small clusters (< ",
            small_pop, " cells)")
  }

  for (sc in small_clusters) {
    cells <- which(labels == sc)
    for (cell in cells) {
      nbr_labels <- labels[neighbor_array[cell, ]]
      # Filter to non-small clusters
      avail <- nbr_labels[!nbr_labels %in% small_clusters]
      if (length(avail) > 0) {
        # Most frequent non-small neighbor label
        best <- .mode_val(avail)
        labels[cell] <- best
      }
    }
  }

  # Iterative pass: merge remaining small clusters (any neighbor, time-limited)
  t_start <- proc.time()[3]

  repeat {
    tab <- table(labels)
    small_clusters <- as.integer(names(tab)[tab < small_pop])
    if (length(small_clusters) == 0) break
    if ((proc.time()[3] - t_start) > time_smallpop) break

    for (sc in small_clusters) {
      cells <- which(labels == sc)
      for (cell in cells) {
        nbr_labels <- labels[neighbor_array[cell, ]]
        best <- .mode_val(nbr_labels)
        labels[cell] <- best
      }
    }
  }

  # Relabel contiguously
  as.integer(factor(labels))
}


#' Mode of a vector (most frequent value, ties broken by first occurrence)
#' @keywords internal
.mode_val <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
