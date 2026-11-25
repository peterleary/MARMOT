g <- function(...) intToUtf8(c(...))   # Greek letters by code point (the file stays ASCII)

test_that("names with Greek letters become plotmath; others stay as they are", {
  x <- c(paste0("IFN-", g(0x3b3)), "CD4", paste0("TCR", g(0x3b3, 0x3b4)), NA)
  e <- greek_label(x)
  expect_true(is.expression(e))
  expect_length(e, 4)
  expect_identical(e[[1]], quote("IFN-" * gamma))
  expect_identical(e[[2]], "CD4")
  expect_identical(e[[3]], quote("TCR" * gamma * delta))
  expect_identical(greek_label(c("CD4", "CD8")), c("CD4", "CD8"))
  expect_identical(greek_label(g(0xb5)), as.expression(quote(mu)))   # the micro sign too
})

test_that("Greek letters are spelt out where plotmath cannot go", {
  expect_identical(spell_greek(c(paste0("CD8", g(0x3b1)), "CD4", NA)), c("CD8alpha", "CD4", NA))
  expect_identical(spell_greek(paste0(g(0x3b3, 0x3b4), " T cells")), "gammadelta T cells")
})

test_that("markers show their display names, Greek letters and all", {
  e <- marker_label(c("IFNg", "CD4", "CD8a"), display = c(IFNg = paste0("IFN-", g(0x3b3)), CD4 = "", CD8a = NA))
  expect_identical(e[[1]], quote("IFN-" * gamma))
  expect_identical(e[[2]], "CD4")
  expect_identical(e[[3]], "CD8a")
  expect_identical(marker_label(c("CD4", "CD19")), c("CD4", "CD19"))
})

test_that("the facet labeller and a PDF draw Greek letters without warnings", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:2, y = 1:2, f = c(paste0(g(0x3b3, 0x3b4), " T cells"), "B cells"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    ggplot2::facet_wrap(~f, labeller = label_greek) +
    ggplot2::labs(title = greek_label(paste0("IFN-", g(0x3b3))))
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  expect_no_warning(print(p))
  grDevices::dev.off()
  expect_gt(file.size(path), 1000)
})

test_that("display names as text, and prepared strip labels with two lines", {
  expect_identical(marker_display(c("IFNg", "CD4"), c(IFNg = "IFN-g")), c("IFN-g", "CD4"))
  expect_identical(marker_display("CD4"), "CD4")
  lk <- c(A = paste0("IFN-", g(0x3b3), " \n n = 9"), B = "B \n n = 3")
  e <- greek_label(lk)
  expect_identical(e[[1]], quote(atop(displaystyle("IFN-" * gamma), displaystyle("n = 9"))))
  lab <- greek_labeller(lk)(data.frame(condition = c("A", "B", "C")))
  expect_length(lab$condition, 3)
  expect_identical(greek_labeller(NULL)(data.frame(antigen = c("CD4", "CD8")))$antigen, c("CD4", "CD8"))
  expect_identical(greek_labeller(c(B = "B \n n = 3"))(data.frame(condition = c("B", "C")))$condition, c("B \n n = 3", "C"))
})

test_that("a finished ggplot is relabelled, and left alone when there is nothing to do", {
  skip_if_not_installed("ggplot2")
  treated <- paste0("IFN-", g(0x3b3), " treated")
  df <- data.frame(IFNg = 1:4, CD4 = 4:1, cond = rep(c(treated, "WT"), 2))
  p <- ggplot2::ggplot(df, ggplot2::aes(IFNg, CD4, colour = cond)) + ggplot2::geom_point() +
    ggplot2::facet_wrap(~cond) + ggplot2::ggtitle("IFNg")
  q <- greek_ggplot(p, c(IFNg = paste0("IFN-", g(0x3b3))))
  expect_true(is.expression(ggplot2::get_labs(q)$title))
  expect_true(is.expression(ggplot2::get_labs(q)$x))
  expect_true(q$scales$has_scale("colour"))
  expect_identical(p$facet$params$labeller, ggplot2::label_value)   # the original is unchanged
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  expect_no_warning(print(q))
  grDevices::dev.off()
  plain <- ggplot2::ggplot(data.frame(a = 1:2, b = 1:2, c = c("x", "y")), ggplot2::aes(a, b, colour = c)) + ggplot2::geom_point()
  expect_identical(greek_ggplot(plain), plain)
  expect_length(greek_ggplot(list(plain, plain)), 2)
})

test_that("a finished heatmap gets Greek labels and display names; others are untouched", {
  skip_if_not_installed("ComplexHeatmap")
  m <- matrix(1:6, 2, dimnames = list(c(paste0("IFNg(3)"), "CD4(3)"), c("IFNg", "CD8a", "s3")))
  h <- ComplexHeatmap::Heatmap(m, name = "x")
  r <- greek_heatmap(h, c(IFNg = paste0("IFN-", g(0x3b3))))
  expect_true(is.expression(r@column_names_param$labels))
  expect_true(is.expression(r@row_names_param$labels))
  expect_identical(greek_heatmap(h), h)
})
