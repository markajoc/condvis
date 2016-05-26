context("visualweight")

n <- 250000
p <- 50

dat <- data.frame(
  x1 = factor(sample(letters[1:3], size = n, replace = TRUE)),
  x2 = factor(sample(letters[4:6], size = n, replace = TRUE)),
  x3 = factor(sample(letters[7:9], size = n, replace = TRUE)),
  x4 = factor(sample(letters[10:15], size = n, replace = TRUE)),
  x5 = rnorm(n),
  x6 = rnorm(n),
  x7 = rnorm(n),
  x8 = rnorm(n)
  )[, sample(1:8, p, replace = TRUE)]

test_that("visualweight fails without required inputs", {
  expect_error(visualweight(xc.cond = mtcars, xc = NULL))
  expect_error(visualweight(xc.cond = NULL, xc = NULL))
  expect_error(visualweight(xc.cond = mtcars, xc = mtcars[1, ]))
  expect_error(visualweight(xc.cond = mtcars[1, 1:5], xc = mtcars[, 1:6]))
})

test_that("visualweight returns the right types", {
  expect_is(visualweight(dat[1, ], dat[1:20, ]), "numeric")
  expect_is(visualweight(dat[1:5, ], dat[1:20, ]), "matrix")
})

test_that("identical observations have visual weight one", {
  expect_equivalent(diag(visualweight(dat[1:20, ], dat[1:20, ])), rep(1, 20))
  expect_equal(diag(visualweight(dat[1:10, ], dat[1:5, ])), diag(visualweight(
    dat[1:5, ], dat[1:10, ])))
})

test_that("internal visual weight function returns a function", {
  expect_is(visualweight2(mtcars), "function")
  expect_error(visualweight2())
})

test_that("visualweight is not too slow", {
  takes_less_than(2)(visualweight(xc.cond = dat[1, ], xc = dat))
  takes_less_than(5)(visualweight(xc.cond = dat[1:15, ], xc = dat))
})
