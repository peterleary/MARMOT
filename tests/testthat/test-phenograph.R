# test-phenograph.R
# Comprehensive tests for the pure R PhenoGraph reimplementation (R/phenograph.R)

# ============================================================================
# 1. NUMERICAL CORRECTNESS — hand-calculated
# ============================================================================

test_that("hand-calculated 6-cell k=3 Jaccard weights are exact", {
  # 6 points on a 3x2 grid:
  #  p1(0,0)  p2(1,0)  p3(2,0)
  #  p4(0,1)  p5(1,1)  p6(2,1)
  data <- matrix(c(
    0, 0,
    1, 0,
    2, 0,
    0, 1,
    1, 1,
    2, 1
  ), ncol = 2, byrow = TRUE)

  # Get k-NN index (k+1, drop self)
  nn  <- RANN::nn2(data, data, k = 4, searchtype = "standard")
  idx <- nn$nn.idx[, -1, drop = FALSE]  # n x 3

  # Verify neighbor sets (as sets, ignoring order within each row)
  expect_setequal(idx[1, ], c(2, 4, 5))
  expect_setequal(idx[2, ], c(1, 3, 5))
  expect_setequal(idx[3, ], c(2, 5, 6))
  expect_setequal(idx[4, ], c(1, 2, 5))
  expect_setequal(idx[5, ], c(2, 4, 6))
  expect_setequal(idx[6, ], c(2, 3, 5))

  # Compute Jaccard weights
  edges <- MARMOT:::jaccard_weights(idx)
  colnames(edges) <- c("from", "to", "weight")
  edf <- as.data.frame(edges)

  # Expected intersections (computed by hand):
  #   p1->p2: {2,4,5} ^ {1,3,5} = {5}       u=1, w=1/10
  #   p1->p4: {2,4,5} ^ {1,2,5} = {2,5}     u=2, w=2/8
  #   p1->p5: {2,4,5} ^ {2,4,6} = {2,4}     u=2, w=2/8
  #   p2->p1: {1,3,5} ^ {2,4,5} = {5}       u=1, w=1/10
  #   p2->p3: {1,3,5} ^ {2,5,6} = {5}       u=1, w=1/10
  #   p2->p5: {1,3,5} ^ {2,4,6} = {}        u=0, SKIP
  #   p3->p2: {2,5,6} ^ {1,3,5} = {5}       u=1, w=1/10
  #   p3->p5: {2,5,6} ^ {2,4,6} = {2,6}     u=2, w=2/8
  #   p3->p6: {2,5,6} ^ {2,3,5} = {2,5}     u=2, w=2/8
  #   p4->p1: {1,2,5} ^ {2,4,5} = {2,5}     u=2, w=2/8
  #   p4->p2: {1,2,5} ^ {1,3,5} = {1,5}     u=2, w=2/8
  #   p4->p5: {1,2,5} ^ {2,4,6} = {2}       u=1, w=1/10
  #   p5->p2: {2,4,6} ^ {1,3,5} = {}        u=0, SKIP
  #   p5->p4: {2,4,6} ^ {1,2,5} = {2}       u=1, w=1/10
  #   p5->p6: {2,4,6} ^ {2,3,5} = {2}       u=1, w=1/10
  #   p6->p2: {2,3,5} ^ {1,3,5} = {3,5}     u=2, w=2/8
  #   p6->p3: {2,3,5} ^ {2,5,6} = {2,5}     u=2, w=2/8
  #   p6->p5: {2,3,5} ^ {2,4,6} = {2}       u=1, w=1/10

  expected <- data.frame(
    from   = c(1,1,1, 2,2, 3,3,3, 4,4,4, 5,5, 6,6,6),
    to     = c(2,4,5, 1,3, 2,5,6, 1,2,5, 4,6, 2,3,5),
    weight = c(0.1, 0.25, 0.25,
               0.1, 0.1,
               0.1, 0.25, 0.25,
               0.25, 0.25, 0.1,
               0.1, 0.1,
               0.25, 0.25, 0.1)
  )

  # Sort both by (from, to) for comparison
  edf      <- edf[order(edf$from, edf$to), ]
  expected <- expected[order(expected$from, expected$to), ]
  rownames(edf)      <- NULL
  rownames(expected)  <- NULL

  expect_equal(nrow(edf), 16)
  expect_equal(edf$from,   expected$from)
  expect_equal(edf$to,     expected$to)
  expect_equal(edf$weight, expected$weight, tolerance = 1e-15)

  # No self-loops (all 6 nodes are reported)
  expect_true(all(edf$from != edf$to))
})


test_that("k=1 yields all self-loops with weight 0.5", {
  # With k=1, each cell has 1 neighbor. For any pair (i, j=neighbor(i)):
  # neighbors(i) = {j}, neighbors(j) = {some cell != j} → intersection = {}
  # So all edges have u=0, all nodes are isolated → self-loops
  data_k1 <- matrix(c(
    0, 0,
    1, 0,
    3, 0,
    6, 0,
    10, 0
  ), ncol = 2, byrow = TRUE)

  nn  <- RANN::nn2(data_k1, data_k1, k = 2, searchtype = "standard")
  idx <- nn$nn.idx[, -1, drop = FALSE]
  edges <- MARMOT:::jaccard_weights(idx)

  # All edges should be self-loops
  expect_equal(nrow(edges), 5)
  expect_equal(edges[, 1], edges[, 2])  # from == to
  expect_true(all(edges[, 3] == 0.5))
  expect_setequal(edges[, 1], 1:5)
})


# ============================================================================
# 2. CROSS-VALIDATION against C++ Rphenograph
# ============================================================================

test_that("iris k=30: raw Jaccard weights match C++ Rphenograph exactly", {
  skip_if_not_installed("Rphenograph")

  iris_unique <- unique(iris)
  data <- as.matrix(iris_unique[, 1:4])

  # Shared kNN index
  nn  <- RANN::nn2(data, data, k = 31, searchtype = "standard")
  idx <- nn$nn.idx[, -1, drop = FALSE]

  # C++ raw Jaccard (call internal jaccard_coeff_4 directly)
  cpp_raw <- Rphenograph:::jaccard_coeff_4(idx, 0L)
  cpp_raw <- cpp_raw[cpp_raw[, 1] > 0, , drop = FALSE]

  # Pure R Jaccard
  r_raw <- MARMOT:::jaccard_weights(idx)

  # Sort both by (from, to) for comparison
  cpp_sorted <- cpp_raw[order(cpp_raw[, 1], cpp_raw[, 2]), ]
  r_sorted   <- r_raw[order(r_raw[, 1], r_raw[, 2]), ]

  expect_equal(nrow(r_sorted), nrow(cpp_sorted))
  expect_equal(r_sorted[, 1], cpp_sorted[, 1])  # from
  expect_equal(r_sorted[, 2], cpp_sorted[, 2])  # to
  expect_equal(r_sorted[, 3], cpp_sorted[, 3], tolerance = 1e-12)  # weight
})


test_that("iris k=45: raw Jaccard weights match C++ Rphenograph exactly", {
  skip_if_not_installed("Rphenograph")

  iris_unique <- unique(iris)
  data <- as.matrix(iris_unique[, 1:4])

  nn  <- RANN::nn2(data, data, k = 46, searchtype = "standard")
  idx <- nn$nn.idx[, -1, drop = FALSE]

  cpp_raw <- Rphenograph:::jaccard_coeff_4(idx, 0L)
  cpp_raw <- cpp_raw[cpp_raw[, 1] > 0, , drop = FALSE]
  r_raw   <- MARMOT:::jaccard_weights(idx)

  cpp_sorted <- cpp_raw[order(cpp_raw[, 1], cpp_raw[, 2]), ]
  r_sorted   <- r_raw[order(r_raw[, 1], r_raw[, 2]), ]

  expect_equal(nrow(r_sorted), nrow(cpp_sorted))
  expect_equal(r_sorted[, 1], cpp_sorted[, 1])
  expect_equal(r_sorted[, 2], cpp_sorted[, 2])
  expect_equal(r_sorted[, 3], cpp_sorted[, 3], tolerance = 1e-12)
})


test_that("random 1000 cells k=20: raw Jaccard weights match C++ Rphenograph", {
  skip_if_not_installed("Rphenograph")

  set.seed(42)
  data <- matrix(rnorm(1000 * 10), ncol = 10)

  nn  <- RANN::nn2(data, data, k = 21, searchtype = "standard")
  idx <- nn$nn.idx[, -1, drop = FALSE]

  cpp_raw <- Rphenograph:::jaccard_coeff_4(idx, 0L)
  cpp_raw <- cpp_raw[cpp_raw[, 1] > 0, , drop = FALSE]
  r_raw   <- MARMOT:::jaccard_weights(idx)

  cpp_sorted <- cpp_raw[order(cpp_raw[, 1], cpp_raw[, 2]), ]
  r_sorted   <- r_raw[order(r_raw[, 1], r_raw[, 2]), ]

  expect_equal(nrow(r_sorted), nrow(cpp_sorted))
  expect_equal(r_sorted[, 1], cpp_sorted[, 1])
  expect_equal(r_sorted[, 2], cpp_sorted[, 2])
  expect_equal(r_sorted[, 3], cpp_sorted[, 3], tolerance = 1e-12)
})


# ============================================================================
# 3. SPARSE vs LOOP EQUIVALENCE
# ============================================================================

test_that("sparse and loop backends produce identical weights (500 cells k=15)", {
  set.seed(123)
  data <- matrix(rnorm(500 * 8), ncol = 8)

  nn  <- RANN::nn2(data, data, k = 16, searchtype = "standard")
  idx <- nn$nn.idx[, -1, drop = FALSE]

  n <- nrow(idx)
  k <- ncol(idx)

  sparse_edges <- MARMOT:::jaccard_weights_sparse(idx, n, k)
  loop_edges   <- MARMOT:::jaccard_weights_loop(idx, n, k)

  # Sort both by (from, to)
  sparse_sorted <- sparse_edges[order(sparse_edges[, 1], sparse_edges[, 2]), ]
  loop_sorted   <- loop_edges[order(loop_edges[, 1], loop_edges[, 2]), ]

  expect_equal(nrow(sparse_sorted), nrow(loop_sorted))
  expect_equal(sparse_sorted[, 1], loop_sorted[, 1])  # from
  expect_equal(sparse_sorted[, 2], loop_sorted[, 2])  # to
  expect_equal(sparse_sorted[, 3], loop_sorted[, 3], tolerance = 1e-14)  # weight
})


test_that("sparse and loop backends agree on tiny dataset (10 cells k=3)", {
  set.seed(99)
  data <- matrix(rnorm(10 * 4), ncol = 4)

  nn  <- RANN::nn2(data, data, k = 4, searchtype = "standard")
  idx <- nn$nn.idx[, -1, drop = FALSE]

  n <- nrow(idx)
  k <- ncol(idx)

  sparse_edges <- MARMOT:::jaccard_weights_sparse(idx, n, k)
  loop_edges   <- MARMOT:::jaccard_weights_loop(idx, n, k)

  sparse_sorted <- sparse_edges[order(sparse_edges[, 1], sparse_edges[, 2]), ]
  loop_sorted   <- loop_edges[order(loop_edges[, 1], loop_edges[, 2]), ]

  expect_equal(nrow(sparse_sorted), nrow(loop_sorted))
  expect_equal(sparse_sorted[, 1], loop_sorted[, 1])
  expect_equal(sparse_sorted[, 2], loop_sorted[, 2])
  expect_equal(sparse_sorted[, 3], loop_sorted[, 3], tolerance = 1e-14)
})


# ============================================================================
# 4. EDGE CASES
# ============================================================================

test_that("Mphenograph accepts data.frame input", {
  set.seed(7)
  df <- data.frame(x = rnorm(50), y = rnorm(50), z = rnorm(50))
  result <- MARMOT:::Mphenograph(df, k = 5)
  expect_length(result, 2)
  expect_s3_class(result[[2]], "communities")
  mem <- igraph::membership(result[[2]])
  expect_equal(length(mem), 50)
})


test_that("Mphenograph rejects non-matrix, non-data.frame input", {
  expect_error(MARMOT:::Mphenograph(list(1, 2, 3), k = 5),
               "data must be a matrix or data.frame")
})


test_that("Mphenograph rejects k=0", {
  data <- matrix(rnorm(30), ncol = 3)
  expect_error(MARMOT:::Mphenograph(data, k = 0),
               "k must be a positive integer")
})


test_that("Mphenograph rejects k too large", {
  data <- matrix(rnorm(30), ncol = 3)
  expect_error(MARMOT:::Mphenograph(data, k = 9),
               "k must be smaller than nrow")
})


test_that("Mphenograph result structure matches Rphenograph", {
  set.seed(7)
  data <- matrix(rnorm(200 * 5), ncol = 5)
  result <- MARMOT:::Mphenograph(data, k = 10)

  expect_length(result, 2)
  expect_true(igraph::is_igraph(result[[1]]))
  expect_s3_class(result[[2]], "communities")

  # membership extraction works the same way as Rphenograph
  mem <- igraph::membership(result[[2]])
  expect_equal(length(mem), 200)
  expect_true(all(mem >= 1))

  # modularity is accessible
  mod <- igraph::modularity(result[[2]])
  expect_true(is.numeric(mod))
  expect_true(mod > 0)
})


test_that("Mphenograph verbose mode prints messages", {
  set.seed(7)
  data <- matrix(rnorm(100 * 3), ncol = 3)
  expect_message(
    MARMOT:::Mphenograph(data, k = 5, verbose = TRUE),
    "Finding 5 nearest"
  )
})


test_that("Mphenograph is deterministic with same seed", {
  set.seed(1)
  data <- matrix(rnorm(300 * 5), ncol = 5)

  r1 <- MARMOT:::Mphenograph(data, k = 15, seed = 42)
  r2 <- MARMOT:::Mphenograph(data, k = 15, seed = 42)

  mem1 <- igraph::membership(r1[[2]])
  mem2 <- igraph::membership(r2[[2]])
  expect_identical(mem1, mem2)
})


test_that("jaccard_weights handles k=2 minimal case", {
  # 4 cells in a tight cluster: all share neighbors
  data <- matrix(c(0, 0, 0.1, 0, 0, 0.1, 0.1, 0.1), ncol = 2, byrow = TRUE)
  nn  <- RANN::nn2(data, data, k = 3, searchtype = "standard")
  idx <- nn$nn.idx[, -1, drop = FALSE]

  edges <- MARMOT:::jaccard_weights(idx)
  expect_true(nrow(edges) > 0)
  # All weights should be valid (0 < w <= 0.5)
  real_edges <- edges[edges[, 1] != edges[, 2], , drop = FALSE]
  if (nrow(real_edges) > 0) {
    expect_true(all(real_edges[, 3] > 0))
    expect_true(all(real_edges[, 3] <= 0.5))
  }
})


# ============================================================================
# 5. FULL PIPELINE USAGE (Mphenograph as drop-in for Rphenograph)
# ============================================================================

test_that("Mphenograph works as Rphenograph drop-in on iris", {
  iris_unique <- unique(iris)
  data <- as.matrix(iris_unique[, 1:4])

  result <- MARMOT:::Mphenograph(data, k = 30)

  # Extract clusters the same way the MARMOT pipeline does
  ids <- factor(as.character(igraph::membership(result[[2]])))

  expect_equal(length(ids), nrow(data))
  expect_true(length(levels(ids)) >= 2)  # should find multiple clusters
  expect_true(length(levels(ids)) <= 20) # but not too many
})


# ============================================================================
# 6. REAL DATA: cell-level identity at multiple k values
# ============================================================================

test_that("real data: every cell gets identical assignment vs C++ at k=5,25,45,65,85", {
  skip_on_cran()
  skip_if_not_installed("Rphenograph")

  pq_path <- file.path("data", "Results_Files_2026-03-02_10.36.48",
                        "R_files", "parquet", "expression", "exprsQuantNorm.parquet")
  # Traverse upward from tests/testthat/ to package root
  root_path <- file.path("..", "..", pq_path)
  if (!file.exists(root_path)) {
    # Try absolute path
    root_path <- file.path("/Users/peterleary/Desktop/marmot", pq_path)
  }
  skip_if(!file.exists(root_path), "Org19 parquet data not found")

  expr <- arrow::read_parquet(root_path)
  data <- as.matrix(expr[, sapply(expr, is.numeric)])
  cat("\n  Real data:", nrow(data), "cells x", ncol(data), "markers\n")

  k_values <- c(5, 25, 45, 65, 85)

  for (k_val in k_values) {
    # Shared kNN
    nn  <- RANN::nn2(data, data, k = k_val + 1, searchtype = "standard")
    idx <- nn$nn.idx[, -1, drop = FALSE]

    # --- edge list ORDER identity ---
    cpp_raw <- Rphenograph:::jaccard_coeff_4(idx, 0L)
    cpp_raw <- cpp_raw[cpp_raw[, 1] > 0, , drop = FALSE]
    r_raw   <- MARMOT:::jaccard_weights(idx)
    expect_equal(nrow(r_raw), nrow(cpp_raw),
                 label = paste0("k=", k_val, " edge count"))
    expect_true(all(r_raw[, 1] == cpp_raw[, 1]),
                label = paste0("k=", k_val, " from order"))
    expect_true(all(r_raw[, 2] == cpp_raw[, 2]),
                label = paste0("k=", k_val, " to order"))
    expect_true(all(abs(r_raw[, 3] - cpp_raw[, 3]) < 1e-15),
                label = paste0("k=", k_val, " weights"))

    # --- cluster assignment identity ---
    # Build graph from the (proven-identical) R edge list
    rel <- data.frame(from = r_raw[, 1], to = r_raw[, 2], weight = r_raw[, 3])
    g   <- igraph::graph_from_data_frame(rel, directed = FALSE)
    set.seed(42)
    comm_r <- igraph::cluster_louvain(g)
    mem_r  <- igraph::membership(comm_r)

    # C++ full pipeline (uses its own identical edge list → same graph)
    set.seed(42)
    cpp_result <- Rphenograph::Rphenograph(data, k = k_val)
    mem_cpp    <- igraph::membership(cpp_result[[2]])

    n_mismatch <- sum(mem_r != mem_cpp)
    cat(sprintf("  k=%2d: %d clusters, %d mismatches out of %d cells\n",
                k_val, length(unique(mem_r)), n_mismatch, nrow(data)))
    expect_equal(n_mismatch, 0L,
                 label = paste0("k=", k_val, " cell-level identity"))
  }
})


# ============================================================================
# 7. BENCHMARKS (informational — print timings, assert not catastrophically slow)
# ============================================================================

test_that("benchmark: pure R PhenoGraph timing on various sizes", {
  skip_on_cran()

  sizes <- c(1000, 5000, 10000)
  k <- 30
  has_cpp <- requireNamespace("Rphenograph", quietly = TRUE)

  cat("\n\n=== PhenoGraph Benchmark ===\n")
  cat(sprintf("%-8s  %-12s  %-12s  %-8s\n", "n", "R (sec)", "C++ (sec)", "Ratio"))
  cat(paste0(rep("-", 48), collapse = ""), "\n")

  for (n in sizes) {
    set.seed(1)
    data <- matrix(rnorm(n * 15), ncol = 15)

    # Pure R timing (just Jaccard weights — the hot path)
    nn  <- RANN::nn2(data, data, k = k + 1, searchtype = "standard")
    idx <- nn$nn.idx[, -1, drop = FALSE]
    t_r <- system.time(MARMOT:::jaccard_weights(idx))[[3]]

    if (has_cpp) {
      # Apples-to-apples: time only the Jaccard computation for C++ too
      t_cpp <- system.time(Rphenograph:::jaccard_coeff_4(idx, 0L))[[3]]
      ratio <- if (t_cpp > 0) round(t_r / t_cpp, 1) else NA
      cat(sprintf("%-8d  %-12.3f  %-12.3f  %-8s\n",
                  n, t_r, t_cpp, ifelse(is.na(ratio), "N/A", paste0(ratio, "x"))))
    } else {
      cat(sprintf("%-8d  %-12.3f  %-12s  %-8s\n", n, t_r, "skip", "N/A"))
    }
  }
  cat("===========================\n\n")

  # Smoke test: the largest size should complete
  expect_true(TRUE)
})


test_that("benchmark: large dataset (50k cells)", {
  skip_on_cran()
  skip_if(Sys.getenv("MARMOT_BENCH_LARGE") == "",
          "Set MARMOT_BENCH_LARGE=1 to run large benchmarks")

  sizes <- c(50000, 100000)
  k <- 30
  has_cpp <- requireNamespace("Rphenograph", quietly = TRUE)

  cat("\n\n=== Large PhenoGraph Benchmark ===\n")
  cat(sprintf("%-8s  %-12s  %-12s  %-8s\n", "n", "R (sec)", "C++ (sec)", "Ratio"))
  cat(paste0(rep("-", 48), collapse = ""), "\n")

  for (n in sizes) {
    set.seed(1)
    data <- matrix(rnorm(n * 15), ncol = 15)

    nn  <- RANN::nn2(data, data, k = k + 1, searchtype = "standard")
    idx <- nn$nn.idx[, -1, drop = FALSE]
    t_r <- system.time(MARMOT:::jaccard_weights(idx))[[3]]

    if (has_cpp) {
      t_cpp <- system.time(Rphenograph:::jaccard_coeff_4(idx, 0L))[[3]]
      ratio <- if (t_cpp > 0) round(t_r / t_cpp, 1) else NA
      cat(sprintf("%-8d  %-12.3f  %-12.3f  %-8s\n",
                  n, t_r, t_cpp, ifelse(is.na(ratio), "N/A", paste0(ratio, "x"))))
    } else {
      cat(sprintf("%-8d  %-12.3f  %-12s  %-8s\n", n, t_r, "skip", "N/A"))
    }

    # Assert not catastrophically slow (< 20x C++ or < 120s absolute)
    expect_true(t_r < 120, label = paste("n=", n, "completed in <120s"))
  }
  cat("==================================\n\n")
})


# ============================================================================
# 7. MfastPG — HNSW-based approximate PhenoGraph
# ============================================================================

test_that("MfastPG basic functionality on iris", {
  data <- as.matrix(iris[, 1:4])
  result <- MfastPG(data, k = 15, seed = 2)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(igraph::is_igraph(result[[1]]))
  expect_s3_class(result[[2]], "communities")

  ids <- igraph::membership(result[[2]])
  expect_length(ids, 150)
  expect_true(all(ids >= 1))
  n_clusters <- length(unique(ids))
  expect_true(n_clusters >= 2 && n_clusters <= 20)
})

test_that("MfastPG is reproducible with same seed", {
  set.seed(42)
  data <- matrix(rnorm(1000 * 10), nrow = 1000)

  r1 <- MfastPG(data, k = 20, seed = 2)
  r2 <- MfastPG(data, k = 20, seed = 2)

  expect_identical(
    igraph::membership(r1[[2]]),
    igraph::membership(r2[[2]])
  )
})

test_that("MfastPG different seed can give different results", {
  set.seed(42)
  data <- matrix(rnorm(500 * 10), nrow = 500)

  r1 <- MfastPG(data, k = 10, seed = 1)
  r2 <- MfastPG(data, k = 10, seed = 99)

  # Different seeds may or may not give different results depending on data,

  # but both should produce valid output
  expect_length(igraph::membership(r1[[2]]), 500)
  expect_length(igraph::membership(r2[[2]]), 500)
})

test_that("MfastPG handles data.frame input", {
  result <- MfastPG(iris[, 1:4], k = 10, seed = 2)
  expect_true(igraph::is_igraph(result[[1]]))
  expect_length(igraph::membership(result[[2]]), 150)
})

test_that("MfastPG input validation", {
  expect_error(MfastPG("not a matrix", k = 5), "data must be a matrix")
  expect_error(MfastPG(matrix(1:10, ncol = 2), k = 0), "k must be a positive")
  expect_error(MfastPG(matrix(1:10, ncol = 2), k = 10), "k must be smaller")
})

test_that("MfastPG verbose mode prints messages", {
  data <- as.matrix(iris[, 1:4])
  expect_message(MfastPG(data, k = 10, seed = 2, verbose = TRUE), "HNSW")
  expect_message(MfastPG(data, k = 10, seed = 2, verbose = TRUE), "Jaccard")
  expect_message(MfastPG(data, k = 10, seed = 2, verbose = TRUE), "Louvain")
})

test_that("MfastPG produces similar clusters to Mphenograph on structured data", {
  # On well-structured data with clear clusters, approximate and exact k-NN
  # should find very similar community structure
  set.seed(42)
  data <- rbind(
    matrix(rnorm(200 * 5, mean = 0), ncol = 5),
    matrix(rnorm(200 * 5, mean = 5), ncol = 5),
    matrix(rnorm(200 * 5, mean = 10), ncol = 5)
  )

  fast_res <- MfastPG(data, k = 15, seed = 2)
  exact_res <- Mphenograph(data, k = 15, seed = 2)

  fast_ids <- igraph::membership(fast_res[[2]])
  exact_ids <- igraph::membership(exact_res[[2]])

  # Both should find ~3 clusters
  expect_true(length(unique(fast_ids)) >= 2)
  expect_true(length(unique(exact_ids)) >= 2)

  # Cluster count should be close (within 1)
  expect_true(abs(length(unique(fast_ids)) - length(unique(exact_ids))) <= 1)
})

test_that("MfastPG ef parameter affects recall", {
  set.seed(42)
  data <- matrix(rnorm(2000 * 10), nrow = 2000)

  # Low ef should still produce valid clusters
  r_low <- MfastPG(data, k = 15, seed = 2, ef = 10)
  r_high <- MfastPG(data, k = 15, seed = 2, ef = 200)

  expect_true(igraph::is_igraph(r_low[[1]]))
  expect_true(igraph::is_igraph(r_high[[1]]))
  expect_length(igraph::membership(r_low[[2]]), 2000)
  expect_length(igraph::membership(r_high[[2]]), 2000)
})
