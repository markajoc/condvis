context("interpolate")

test_that("interpolate throws an error with a negative 'ninterp'", {
  expect_error(interpolate(1:5, ninterp = -1))
})

test_that("interpolate works on a numeric vector of length 2", {
  expect_equal(interpolate(x = 1:2, ninterp = 3), c(1, 1.25, 1.50, 1.75, 2))
})
