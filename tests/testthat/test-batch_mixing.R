test_that("separate batches score near 0 and mixed batches near 1", {
  set.seed(1)
  apart <- rbind(matrix(rnorm(1000, 0), ncol = 2), matrix(rnorm(1000, 20), ncol = 2))
  mixed <- matrix(rnorm(2000), ncol = 2)
  b <- rep(c("b1", "b2"), each = 500)
  expect_lt(batch_mixing(apart, b), 0.05)
  expect_gt(batch_mixing(mixed, sample(b)), 0.85)
})

test_that("the score repeats and leaves the random-number stream alone", {
  set.seed(5); before <- .Random.seed
  x <- matrix(rnorm(400), ncol = 2); b <- rep(c("a", "b"), 100)
  set.seed(5)
  s1 <- batch_mixing(x, b, n = 50)
  expect_identical(.Random.seed, before)
  expect_identical(batch_mixing(x, b, n = 50), s1)
})
