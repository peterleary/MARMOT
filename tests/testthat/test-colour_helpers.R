# Tests for inst/app/helpers/colour_helpers.R

test_that("catalystCols has 60 entries", {
  expect_length(catalystCols, 60)
})

test_that("brewerCols has 56 entries", {
  expect_length(brewerCols, 56)
})

test_that("viridisColours has 8 named palettes", {
  expect_length(viridisColours, 8)
  expect_true(all(c("viridis", "magma", "inferno", "plasma") %in% viridisColours))
})

test_that("scicoColours has 8 named palettes", {
  expect_length(scicoColours, 8)
  expect_true(all(c("bam", "berlin", "vikO") %in% scicoColours))
})

test_that("apply_continuous_scale with viridis palette adds scale", {
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10, z = 1:10),
                       ggplot2::aes(x = x, y = y, colour = z)) +
    ggplot2::geom_point()
  p2 <- apply_continuous_scale(p, "viridis", direction = 1, aesthetic = "colour")
  scales_list <- p2$scales$scales
  has_colour_scale <- any(vapply(scales_list, function(s) {
    "colour" %in% s$aesthetics
  }, logical(1)))
  expect_true(has_colour_scale)
})

test_that("apply_continuous_scale with scico palette works", {
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10, z = 1:10),
                       ggplot2::aes(x = x, y = y, colour = z)) +
    ggplot2::geom_point()
  p2 <- apply_continuous_scale(p, "bam", direction = 1, aesthetic = "colour")
  scales_list <- p2$scales$scales
  has_colour_scale <- any(vapply(scales_list, function(s) {
    "colour" %in% s$aesthetics
  }, logical(1)))
  expect_true(has_colour_scale)
})

test_that("apply_continuous_scale with unknown palette returns plot unchanged", {
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10, z = 1:10),
                       ggplot2::aes(x = x, y = y, colour = z)) +
    ggplot2::geom_point()
  p2 <- apply_continuous_scale(p, "totally_not_a_palette", direction = 1, aesthetic = "colour")
  expect_equal(length(p$scales$scales), length(p2$scales$scales))
})

test_that("apply_continuous_scale with fill aesthetic works", {
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10, z = 1:10),
                       ggplot2::aes(x = x, y = y, fill = z)) +
    ggplot2::geom_point(shape = 21)
  p2 <- apply_continuous_scale(p, "viridis", direction = 1, aesthetic = "fill")
  scales_list <- p2$scales$scales
  has_fill_scale <- any(vapply(scales_list, function(s) {
    "fill" %in% s$aesthetics
  }, logical(1)))
  expect_true(has_fill_scale)
})

test_that("diverging palettes are recognized", {
  expect_true(length(divergingColours) > 0)
  expect_true("RdBu" %in% divergingColours)
})
