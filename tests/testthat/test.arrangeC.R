context("arrangeC")

test_that("arrangeC fails with less than 5 rows", {
  expect_error(arrangeC(mtcars[1:4, ]))
})
