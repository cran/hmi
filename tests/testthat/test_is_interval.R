context("is_interval")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("is_interval dectecs intervals correctly", {
  expect_false(is_interval(rnorm(10)))
  expect_false(is_interval(1:10))
  expect_false(is_interval(c("m", "f", "m")))
  expect_false(is_interval(c("100", "200", "150")))
  expect_false(is_interval(c("100", "200", "150;200")))
  expect_true(is_interval(c("100;100", "200;200", "150;200")))
  expect_true(is_interval(c("100;100", "200;200", NA, "150;200")))
  expect_true(is_interval(c("100;100", "-Inf;-Inf", "150;200")))
  expect_true(is_interval(c("100;100", "-Inf;0", "150;200")))
  expect_true(is_interval(c("100;100", "0;Inf", "150;200")))
  expect_true(is_interval(c("100;100", "Inf;Inf", "150;200")))
})
