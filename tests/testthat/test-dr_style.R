test_that("axis names follow the map", {
  expect_identical(dr_axis_names("UMAP"), c("UMAP1", "UMAP2"))
  expect_identical(dr_axis_names("Downsampled.TSNE"), c("tSNE1", "tSNE2"))
  expect_identical(dr_axis_names("UMAP_uncorrected"), c("UMAP1", "UMAP2"))
  expect_identical(dr_axis_names("PCA"), c("PC1", "PC2"))
  expect_identical(dr_axis_names("Mpacmap"), c("PaCMAP1", "PaCMAP2"))
})

test_that("corner arrows sit just outside the cells, equal in length, with a fixed ratio", {
  emb <- data.frame(x = c(0, 10), y = c(0, 4))
  layers <- dr_corner_arrows(emb)
  expect_length(layers, 5)
  expect_identical(layers[[5]]$ratio, 1)
  seg <- layers[[1]]$data
  expect_equal(c(seg$x, seg$xend - seg$x), c(-0.2, 1.5))
  vert <- layers[[2]]$data
  expect_equal(c(vert$y, vert$yend - vert$y), c(-0.08, 1.5))
  expect_length(dr_corner_arrows(emb, fixed = FALSE), 4)
})
