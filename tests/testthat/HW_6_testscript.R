library(testthat)
library(sparsevector)

context("sparsevector: sparse_numeric class methods")

#------------------------
# Validity & coercion
#------------------------
test_that("sparse object with invalid pos gives error", {
  expect_error(
    new("sparse_numeric", value = c(1, 2), pos = c(1L, 4L), length = 3L)
  )
})

test_that("coercion to numeric returns correct vector", {
  x <- new("sparse_numeric", value = c(5, 3), pos = c(2L, 5L), length = 6L)
  dense <- as(x, "numeric")
  expect_equal(dense, c(0, 5, 0, 0, 3, 0))
})

#------------------------
# Arithmetic: sparse_add / +
#------------------------
test_that("sparse_add handles overlapping and non-overlapping positions", {
  x <- as(c(0,2,0,3,0), "sparse_numeric")
  y <- as(c(1,0,4,0,0), "sparse_numeric")
  result <- as(c(1,2,4,3,0), "sparse_numeric")
  expect_equal(sparse_add(x,y), result)
  expect_equal(x + y, result)  # operator
})

#------------------------
# sparse_sub / -
#------------------------
test_that("sparse_sub removes zeros after subtraction", {
  x <- as(c(1,2,0,3,0), "sparse_numeric")
  y <- as(c(1,1,0,4,0), "sparse_numeric")
  diff <- sparse_sub(x, y)
  expect_equal(as(diff, "numeric"), c(0,1,0,-1,0))
  expect_equal(x - y, diff) # operator
})

#------------------------
# sparse_mult / *
#------------------------
test_that("sparse_mult multiplies correctly", {
  x <- as(c(0,2,0,3), "sparse_numeric")
  y <- as(c(1,0,5,2), "sparse_numeric")
  result <- as(c(0,0,0,6), "sparse_numeric")
  expect_equal(sparse_mult(x, y), result)
  expect_equal(x * y, result)
})

#------------------------
# sparse_crossprod
#------------------------
test_that("sparse_crossprod computes dot product correctly", {
  x <- as(c(1,2,0,3), "sparse_numeric")
  y <- as(c(0,2,5,1), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 7) # 2*2 + 3*1 = 7
})

#------------------------
# sparse_sum
#------------------------
test_that("sparse_sum computes sum of stored values", {
  x <- as(c(1,0,2,0,3), "sparse_numeric")
  expect_equal(sparse_sum(x), 6)
})

#------------------------
# mean
#------------------------
test_that("mean works including zeros", {
  x <- as(c(1,0,2,0,3), "sparse_numeric")
  expect_equal(mean(x), 6 / 5)
})

#------------------------
# norm
#------------------------
test_that("norm computes Euclidean norm", {
  x <- as(c(3,0,4), "sparse_numeric")
  expect_equal(sparse_norm(x), 5)
})

#------------------------
# standardize
#------------------------
test_that("standardize errors on zero SD", {
  x <- as(rep(5,3), "sparse_numeric")
  expect_error(standardize(x))
})

#------------------------
# sparse_add edge cases
#------------------------
test_that("sparse_add with empty vectors", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=as.integer(5))
  y <- as(c(1,0,2,0,0), "sparse_numeric")
  expect_equal(sparse_add(x, y), y)
  expect_equal(sparse_add(y, x), y)
})

#------------------------
# sparse_sub edge cases
#------------------------
test_that("sparse_sub with empty vectors", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=as.integer(5))
  y <- as(c(1,0,2,0,0), "sparse_numeric")
  expect_equal(sparse_sub(x, y), new("sparse_numeric", value=-y@value, pos=y@pos, length=as.integer(5)))
  expect_equal(sparse_sub(y, x), y)
})


#------------------------
# sparse_mult edge cases
#------------------------
test_that("sparse_mult with empty vectors", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=as.integer(5))
  y <- as(c(1,2,3,4,5), "sparse_numeric")
  expect_equal(sparse_mult(x, y), new("sparse_numeric", value=numeric(0), pos=integer(0), length=as.integer(5)))
  expect_equal(sparse_mult(y, x), new("sparse_numeric", value=numeric(0), pos=integer(0), length=as.integer(5)))
})

#------------------------
# sparse_crossprod edge cases
#------------------------
test_that("sparse_crossprod with empty vectors", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=as.integer(3))
  y <- as(c(1,2,3), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 0)
  expect_equal(sparse_crossprod(y, x), 0)
})

#------------------------
# sparse_norm edge cases
#------------------------
test_that("sparse_norm with empty vector", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=as.integer(5))
  expect_equal(sparse_norm(x), 0)
})

#------------------------
# mean edge cases
#------------------------
test_that("mean with empty sparse_numeric returns NaN", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=as.integer(0))
  expect_true(is.nan(mean(x)))
})

#------------------------
# standardize edge case (non-zero SD)
#------------------------
test_that("standardize works when SD non-zero", {
  x <- as(c(1,0,1), "sparse_numeric")
  result <- standardize(x)
  # check mean is ~0 and SD ~1 (scaled)
  expect_equal(round(mean(as(result, "numeric"))), 0)
  expect_true(all(!is.na(as(result, "numeric"))))
})

#------------------------
# sparse_add/sub/mult with single-element vectors
#------------------------
test_that("arithmetic with single-element vectors", {
  x <- as(c(0,0,5), "sparse_numeric")
  y <- as(c(0,2,0), "sparse_numeric")

  expect_equal(as(sparse_add(x,y), "numeric"), c(0,2,5))
  expect_equal(as(sparse_sub(x,y), "numeric"), c(0,-2,5))
  expect_equal(as(sparse_mult(x,y), "numeric"), c(0,0,0))
})

#------------------------
# sort
#------------------------
test_that("sort orders by pos", {
  x <- new("sparse_numeric", value = c(3,1,2), pos = c(5L,2L,4L), length = 6L)
  sorted <- sort(x)
  expect_equal(sorted@pos, c(2L,4L,5L))
  expect_equal(sorted@value, c(1,2,3))
})

#------------------------
# show / plot
#------------------------
test_that("show prints without error", {
  x <- as(c(1,0,2), "sparse_numeric")
  expect_output(show(x))
})

test_that("plot runs without error", {
  x <- as(c(1,0,2), "sparse_numeric")
  y <- as(c(0,2,2), "sparse_numeric")
  expect_silent(plot(x, y))
})
