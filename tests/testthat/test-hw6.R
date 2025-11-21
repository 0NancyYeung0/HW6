library(testthat)
library(Homework6Package)


# Validity & coercion

test_that("sparse object with invalid pos gives error", {
  expect_error(new("sparse_numeric",
                   value = c(1, 2),
                   pos   = c(1L, 4L),
                   length = 3L))
})

test_that("coercion to numeric returns correct vector", {
  x <- new("sparse_numeric", value = c(5, 3), pos = c(2L, 5L), length = 6L)
  expect_equal(as(x, "numeric"), c(0, 5, 0, 0, 3, 0))
})


# sparse_add / +

test_that("sparse_add handles overlapping and non-overlapping positions", {
  x <- as(c(0,2,0,3,0), "sparse_numeric")
  y <- as(c(1,0,4,0,0), "sparse_numeric")
  result <- as(c(1,2,4,3,0), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
  expect_equal(x + y, result)
})

test_that("sparse_add early return cases work", {
  a <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 4L)
  b <- new("sparse_numeric", value = c(2), pos = c(3L), length = 4L)

  expect_equal(sparse_add(a, b)@value, b@value)
  expect_equal(sparse_add(a, b)@pos,   b@pos)

  expect_equal(sparse_add(b, a)@value, b@value)
  expect_equal(sparse_add(b, a)@pos,   b@pos)
})


# sparse_sub / -

test_that("sparse_sub removes zero results correctly", {
  x <- as(c(1,2,0,3,0), "sparse_numeric")
  y <- as(c(1,1,0,4,0), "sparse_numeric")

  diff <- sparse_sub(x, y)
  expect_equal(as(diff, "numeric"), c(0,1,0,-1,0))
  expect_equal(x - y, diff)
})

test_that("sparse_sub early return & negation branches", {
  x0 <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
  y  <- new("sparse_numeric", value = c(3), pos = c(4L), length = 5L)

  expect_equal(sparse_sub(x0, x0)@value, numeric(0))
  expect_equal(sparse_sub(x0, x0)@pos,   integer(0))

  out <- sparse_sub(x0, y)
  expect_equal(out@value, -y@value)
  expect_equal(out@pos,   y@pos)

  out2 <- sparse_sub(y, x0)
  expect_equal(out2@value, y@value)
  expect_equal(out2@pos,   y@pos)
})

test_that("sparse_mult multiplies correctly", {
  x <- as(c(0,2,0,3), "sparse_numeric")
  y <- as(c(1,0,5,2), "sparse_numeric")
  result <- as(c(0,0,0,6), "sparse_numeric")

  expect_equal(sparse_mult(x, y), result)
  expect_equal(x * y, result)
})

test_that("sparse_crossprod computes dot product correctly", {
  x <- as(c(1,2,0,3), "sparse_numeric")
  y <- as(c(0,2,5,1), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 7)  # 2*2 + 3*1
})

test_that("sparse_sum computes sum of stored values", {
  x <- new("sparse_numeric", value = c(3,4), pos = c(2L,5L), length = 6L)
  expect_equal(sparse_sum(x), 7)
})

test_that("mean handles empty and non-empty vectors", {
  x_empty <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 0L)
  expect_true(is.na(mean(x_empty)))

  x <- new("sparse_numeric", value = c(2,3), pos = c(1L,4L), length = 5L)
  expect_equal(mean(x), (2 + 3)/5)
})

test_that("norm computes Euclidean norm", {
  x <- as(c(3,0,4), "sparse_numeric")
  expect_equal(norm(x), 5)
})

test_that("norm returns zero when no stored values", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 10L)
  expect_equal(norm(x), 0)
})

test_that("standardize errors when sd = 0 and values non-zero", {
  x <- as(rep(5,3), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("standardize returns dense vector when sd=0 & no stored pos", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 4L)
  out <- standardize(x)
  expect_equal(out@pos,   1:4)
  expect_equal(out@value, rep(1,4))
})



test_that("standardize normal case returns dense when zero_val != 0", {
  x <- new("sparse_numeric", value = c(10), pos = c(3L), length = 5L)
  out <- standardize(x)

  mu <- mean(x)
  sd_x <- sqrt((sum(x@value^2) - 5 * mu^2) / 5)
  zero_val <- -mu / sd_x

  if (zero_val == 0) {
    expect_equal(out@pos, 3L)
  } else {
    expect_equal(out@pos, 1:5)
  }
})

test_that(".sanitize_sparse drops zeros & NAs", {
  out <- Homework6Package:::.sanitize_sparse(
    vals = c(0, NA, 5),
    poss = c(1L, 2L, 3L),
    len  = 5L
  )
  expect_equal(out@pos, 3L)
  expect_equal(out@value, 5)
})

test_that("sort orders by pos", {
  x <- as(c(0,0,3,0,1,2), "sparse_numeric")
  sorted <- sort(x)
  expect_equal(sorted@pos,   c(3L,5L,6L))
  expect_equal(sorted@value, c(3,1,2))
})

# show / plot
test_that("show prints without error", {
  expect_output(show(as(c(1,0,2), "sparse_numeric")))
})

test_that("plot runs without error", {
  x <- as(c(1,0,2), "sparse_numeric")
  y <- as(c(0,2,2), "sparse_numeric")
  expect_silent(plot(x,y))
})

test_that("coercing character to sparse_numeric errors", {
  expect_error(as("hello", "sparse_numeric"),
               "Cannot coerce character to sparse_numeric")
})

test_that("plot() with two all-zero sparse vectors produces empty plot and returns invisible NULL", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
  y <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)

  expect_invisible(plot(x, y))
})

test_that("show() prints continuation line when more than 10 non-zero entries", {
  vals <- rep(1, 12)
  pos  <- 1:12

  obj <- new("sparse_numeric", value = vals, pos = as.integer(pos), length = 20L)

  expect_output(show(obj), "more non-zero entries")
})

test_that("show() prints '(all zeros)' when there are no non-zero entries", {
  obj <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 10L)

  expect_output(show(obj), "(all zeros)")
})



# VALIDITY & COERCION

test_that("sparse object with invalid pos gives error", {
  expect_error(new("sparse_numeric",
                   value = c(1, 2),
                   pos   = c(1L, 4L),
                   length = 3L))
})

test_that("coercion to numeric returns correct vector", {
  x <- new("sparse_numeric", value = c(5, 3), pos = c(2L, 5L), length = 6L)
  expect_equal(as(x, "numeric"), c(0, 5, 0, 0, 3, 0))
})

# sparse_add / +

test_that("sparse_add handles overlapping and non-overlapping positions", {
  x <- as(c(0,2,0,3,0), "sparse_numeric")
  y <- as(c(1,0,4,0,0), "sparse_numeric")
  result <- as(c(1,2,4,3,0), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
  expect_equal(x + y, result)
})

test_that("sparse_add early return cases work", {
  a <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 4L)
  b <- new("sparse_numeric", value = c(2), pos = c(3L), length = 4L)

  expect_equal(sparse_add(a, b)@value, b@value)
  expect_equal(sparse_add(a, b)@pos,   b@pos)

  expect_equal(sparse_add(b, a)@value, b@value)
  expect_equal(sparse_add(b, a)@pos,   b@pos)
})

# sparse_sub / -

test_that("sparse_sub removes zero results correctly", {
  x <- as(c(1,2,0,3,0), "sparse_numeric")
  y <- as(c(1,1,0,4,0), "sparse_numeric")

  diff <- sparse_sub(x, y)
  expect_equal(as(diff, "numeric"), c(0,1,0,-1,0))
  expect_equal(x - y, diff)
})

test_that("sparse_sub early-return & negation branches", {
  x0 <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
  y  <- new("sparse_numeric", value = c(3), pos = c(4L), length = 5L)

  expect_equal(sparse_sub(x0, x0)@value, numeric(0))
  expect_equal(sparse_sub(x0, x0)@pos,   integer(0))

  out <- sparse_sub(x0, y)
  expect_equal(out@value, -y@value)
  expect_equal(out@pos,   y@pos)

  out2 <- sparse_sub(y, x0)
  expect_equal(out2@value, y@value)
  expect_equal(out2@pos,   y@pos)
})

# sparse_mult / *

test_that("sparse_mult multiplies correctly", {
  x <- as(c(0,2,0,3), "sparse_numeric")
  y <- as(c(1,0,5,2), "sparse_numeric")
  result <- as(c(0,0,0,6), "sparse_numeric")

  expect_equal(sparse_mult(x, y), result)
  expect_equal(x * y, result)
})

# sparse_crossprod

test_that("sparse_crossprod computes dot product correctly", {
  x <- as(c(1,2,0,3), "sparse_numeric")
  y <- as(c(0,2,5,1), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 7)
})

# sparse_sum

test_that("sparse_sum computes sum of stored values", {
  x <- new("sparse_numeric", value = c(3,4), pos = c(2L,5L), length = 6L)
  expect_equal(sparse_sum(x), 7)
})

# mean

test_that("mean handles empty and non-empty vectors", {
  x_empty <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 0L)
  expect_true(is.na(mean(x_empty)))

  x <- new("sparse_numeric", value = c(2,3), pos = c(1L,4L), length = 5L)
  expect_equal(mean(x), (2 + 3)/5)
})

# norm

test_that("norm computes Euclidean norm", {
  x <- as(c(3,0,4), "sparse_numeric")
  expect_equal(norm(x), 5)
})

test_that("norm returns zero when no stored values", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 10L)
  expect_equal(norm(x), 0)
})

# standardize

test_that("standardize errors when sd = 0 and values non-zero", {
  x <- as(rep(5,3), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("standardize returns dense vector when sd=0 & no stored pos", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 4L)
  out <- standardize(x)
  expect_equal(out@pos,   1:4)
  expect_equal(out@value, rep(1,4))
})

test_that("standardize returns sparse result when zero_val == 0", {
  x <- new("sparse_numeric",
           value = c(-1, 1),
           pos = c(1L, 3L),
           length = 3L)

  out <- standardize(x)

  # Compute expected values using the SAME formula as the implementation
  mu <- mean(x)
  sd_x <- sqrt((sum(x@value^2) - x@length * mu^2) / x@length)
  expected_vals <- (x@value - mu) / sd_x

  expect_s4_class(out, "sparse_numeric")
  expect_equal(out@pos, c(1L, 3L))
  expect_equal(out@value, expected_vals)
})


# sanitize

test_that(".sanitize_sparse drops zeros & NAs", {
  out <- Homework6Package:::.sanitize_sparse(
    vals = c(0, NA, 5),
    poss = c(1L, 2L, 3L),
    len  = 5L
  )
  expect_equal(out@pos, 3L)
  expect_equal(out@value, 5)
})

# sort

test_that("sort orders by pos", {
  x <- as(c(0,0,3,0,1,2), "sparse_numeric")
  sorted <- sort(x)
  expect_equal(sorted@pos,   c(3L,5L,6L))
  expect_equal(sorted@value, c(3,1,2))
})

# show / plot

test_that("show prints without error", {
  expect_output(show(as(c(1,0,2), "sparse_numeric")))
})

test_that("plot runs without error", {
  x <- as(c(1,0,2), "sparse_numeric")
  y <- as(c(0,2,2), "sparse_numeric")
  expect_silent(plot(x,y))
})

test_that("coercing character to sparse_numeric errors", {
  expect_error(as("hello", "sparse_numeric"),
               "Cannot coerce character to sparse_numeric")
})

test_that("plot() with two all-zero sparse vectors produces empty plot and returns invisible NULL", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
  y <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)

  expect_invisible(plot(x, y))
})

test_that("show() prints continuation line when >10 non-zero entries", {
  vals <- rep(1, 12)
  pos  <- 1:12

  obj <- new("sparse_numeric", value = vals, pos = as.integer(pos), length = 20L)

  expect_output(show(obj), "more non-zero entries")
})

test_that("show() prints '(all zeros)' when there are no non-zero entries", {
  obj <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 10L)
  expect_output(show(obj), "(all zeros)")
})

test_that("plot errors when x is not sparse_numeric", {
  bad <- "not_sparse"
  y <- new("sparse_numeric", value = 1, pos = 1L, length = 3L)

  plot_method <- getMethod("plot", c("sparse_numeric", "sparse_numeric"))

  expect_error(
    plot_method(bad, y),
    "x must be sparse_numeric"
  )
})

test_that("vector length mismatch triggers error (line 228)", {
  x <- new("sparse_numeric", value = 1, pos = 1L, length = 3L)
  y <- new("sparse_numeric", value = 2, pos = 2L, length = 5L)

  expect_error(sparse_add(x, y), "vectors must have same length")
  expect_error(sparse_sub(x, y), "vectors must have same length")
  expect_error(sparse_mult(x, y), "vectors must have same length")
  expect_error(sparse_crossprod(x, y), "vectors must have same length")
})
