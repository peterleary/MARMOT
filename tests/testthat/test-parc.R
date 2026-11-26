test_that("shared neighbours in blocks match the whole product exactly", {
  skip_if_not_installed("Matrix")
  set.seed(3)
  n <- 3000
  # a random sparse neighbour graph, made symmetric as Mparc does
  i <- rep(seq_len(n), each = 12)
  j <- sample.int(n, length(i), replace = TRUE)
  keep <- i != j
  a <- Matrix::sparseMatrix(i = i[keep], j = j[keep], x = 1, dims = c(n, n)) > 0
  adj_sym <- a | Matrix::t(a)
  s <- Matrix::summary(Matrix::sparseMatrix(i = i[keep], j = j[keep], x = 1, dims = c(n, n)))
  whole <- Matrix::tcrossprod(adj_sym)[cbind(s$i, s$j)]
  expect_identical(.parc_edge_intersections(adj_sym, s$i, s$j), whole)
  # tiny blocks (1,000 cells each, so three of them) give the same
  expect_identical(.parc_edge_intersections(adj_sym, s$i, s$j, max_entries = 1), whole)
  expect_identical(.parc_edge_intersections(adj_sym, integer(), integer()), numeric())
})
