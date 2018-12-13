context("factors")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("correct factors are returned", {
  expect_equal(factors(1), 1)
  expect_equal(factors(10), c(1, 2, 5, 10))
  expect_true(is.na(factors(1:10)))
})
