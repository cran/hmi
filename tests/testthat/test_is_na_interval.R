context("is.na.interval")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("NAs in intervals are found correctly", {
  expect_equal(is.na.interval(c("100;100", "0;100", "150;200")), c(FALSE, FALSE, FALSE))
  expect_equal(is.na.interval(c("100;100", "0;100", "150;Inf")), c(FALSE, FALSE, FALSE))
  expect_equal(is.na.interval(c("100;100", "0;100", "-Inf;Inf")), c(FALSE, FALSE, TRUE))
  expect_equal(is.na.interval(c("100;100", "0;100", NA)), c(FALSE, FALSE, TRUE))
})
