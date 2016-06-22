context("makepath")

data(powerplant)

ncentroids <- 20
ninterp <- 4
pathobject <- makepath(Xc = powerplant, ncentroids = ncentroids, ninterp =
  ninterp)

test_that("makepath returns dataframes with correct names", {
  expect_equal(names(o$path), names(powerplant))
  expect_equal(names(o$centers), names(powerplant))
})

test_that("makepath returns the correct number of centers and path points", {
  expect_equal(nrow(o$centers), ncentroids)
  expect_equal(nrow(o$path), (ncentroids - 1) * ninterp)
})
