context("arrangeC")

test_that("arrangeC fails with less than 5 rows", {
  expect_error(arrangeC(mtcars[1:4, ]))
})

test_that("arrangeC just returns colnames for ncols less than 3", {
  expect_equal(unlist(arrangeC(mtcars[, 1, drop = FALSE])), colnames(mtcars)[1])
  expect_equal(unlist(arrangeC(mtcars[, 1:2, drop = FALSE])), colnames(mtcars[,
    1:2]))
})

test_that("arrangeC returns list of vectors with length <= 2", {
  expect_is(arrangeC(mtcars), "list")
  expect_true(all(vapply(arrangeC(mtcars), length, integer(1)) <= 2))
})
