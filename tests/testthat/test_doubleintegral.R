context("doubleintegral")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("doubleintegral works", {
  expect_equal(doubleintegral(lower_inner = -Inf, upper_inner = Inf, lower_outer = 0, upper_outer = 0, pbivnormX), 0)
  expect_equal(doubleintegral(lower_inner = 0, upper_inner = 0, lower_outer = -1.32, upper_outer = 1.44, pbivnormX), 0)
  expect_equal(doubleintegral(lower_inner = 0, upper_inner = 1, lower_outer = 2, upper_outer = 3, pbivnormX), 0.007304857)
})
