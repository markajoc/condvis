context("similarityweight")

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

test_that("similarityweight fails without required inputs", {
  expect_error(similarityweight(x = mtcars, data = NULL))
  expect_error(similarityweight(x = NULL, data = NULL))
  expect_error(similarityweight(x = mtcars, data = mtcars[1, ]))
})

test_that("similarityweight returns the right types", {
  expect_is(similarityweight(x = dat[1, ], data = dat[1:20, ]), "numeric")
  expect_is(similarityweight(x = dat[1:5, ], data = dat[1:20, ]), "matrix")
})

test_that("identical observations have weight one", {
  expect_equivalent(diag(similarityweight(x = dat[1:20, ], data = dat[1:20, ])),
    rep(1, 20))
  expect_equal(diag(similarityweight(x = dat[1:10, ], data = dat[1:5, ])),
    diag(similarityweight(x = dat[1:5, ], data = dat[1:10, ])))
})

data(powerplant)
test_that("larger threshold values give equal or larger weights", {
  expect_true(all(similarityweight(x = powerplant[1:50, ], data = powerplant,
    threshold = 0.2) >= similarityweight(x = powerplant[1:50, ], data =
    powerplant, threshold = 0.1)))
  expect_true(all(similarityweight(x = powerplant[1:50, ], data = powerplant,
    threshold = 0.5) >= similarityweight(x = powerplant[1:50, ], data =
    powerplant, threshold = 0.2)))
  expect_true(all(similarityweight(x = powerplant[1:50, ], data = powerplant,
    threshold = 3) >= similarityweight(x = powerplant[1:50, ], data = powerplant
    , threshold = 1)))
})

test_that("setting threshold to Inf gives weight one to everything", {
  expect_true(all(similarityweight(x = powerplant[1:50, ], data = powerplant,
    threshold = Inf) == 1))
  expect_true(all(similarityweight(x = dat[1, ], data = dat, threshold = Inf)
    == 1))
})

test_that("internal visual weight function returns a function", {
  expect_is(.similarityweight(mtcars), "function")
  expect_error(.similarityweight())
})

test_that("similarityweight is not too slow", {
  takes_less_than(3)(similarityweight(x = dat[1, ], data = dat))
  takes_less_than(6)(similarityweight(x = dat[1:15, ], data = dat))
})
