context("suggest_rounding_degrees")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("correct rounding degrees are suggested", {
  expect_equal(suggest_rounding_degrees(c(3, 6, 7, 6, 6, 10, 12, 4, 12, 18, 18, 36, 36)), c(1, 6))
})
