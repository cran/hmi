context("fixed_intercept_check")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("correct intercept variables are detected", {
  expect_true(fixed_intercept_check(y ~ .))
  expect_false(fixed_intercept_check(y ~ 0 + .))
  expect_false(fixed_intercept_check(y ~ -1 + .))
  expect_true(fixed_intercept_check(y ~ x + .))
  expect_false(fixed_intercept_check(y ~ 0 + x + .))
  expect_false(fixed_intercept_check(y ~ -1 + x + .))
  expect_true(fixed_intercept_check(y ~ x))
  expect_false(fixed_intercept_check(y ~ 0 + x))
  expect_false(fixed_intercept_check(y ~ -1 + x))
})
