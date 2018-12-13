context("cleanup")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("data are cleaned correctly", {
  expect_equal(cleanup(iris, k = 10), iris)
  expect_equal(cleanup(iris, k = 2), iris[, -5])
  expect_equal(cleanup(InsectSprays, k = 10), InsectSprays)
  expect_equal(cleanup(InsectSprays, k = 5), InsectSprays[, -2, drop = FALSE])
})
