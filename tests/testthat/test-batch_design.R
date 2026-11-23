ids <- paste0("S", 1:8)
cond <- rep(c("A", "B"), each = 4)

test_that("a balanced design passes with no notes", {
  expect_identical(check_batch_design(rep(c("b1", "b2"), 4), cond, ids), character())
})

test_that("blank batches stop the run, naming the samples", {
  b <- rep(c("b1", "b2"), 4); b[3] <- NA
  expect_error(check_batch_design(b, cond, ids), "S3")
})

test_that("a single batch stops the run", {
  expect_error(check_batch_design(rep("b1", 8), cond, ids), "same batch")
})

test_that("a batch with one sample stops the run", {
  expect_error(check_batch_design(c(rep("b1", 7), "b2"), cond, ids), "only one sample")
})

test_that("batch identical to condition stops the run", {
  expect_error(check_batch_design(rep(c("b1", "b2"), each = 4), cond, ids), "same thing")
})

test_that("batches nested in conditions stop the run", {
  expect_error(check_batch_design(c("b1", "b1", "b2", "b2", "b3", "b3", "b3", "b3"), cond, ids), "impossible to tell apart")
})

test_that("too few samples stop the run", {
  expect_error(check_batch_design(c("b1", "b2", "b1", "b2"), c("A", "A", "B", "B"), ids[1:4]), "Too few samples")
})

test_that("a batch holding one condition is noted, not stopped", {
  b <- c("b1", "b2", "b1", "b2", "b1", "b2", "b3", "b3")
  notes <- check_batch_design(b, cond, ids)
  expect_length(notes, 1)
  expect_match(notes, "'b3'", fixed = TRUE)
})
