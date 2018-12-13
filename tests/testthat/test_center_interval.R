context("center_interval")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("intervals are centered correctly", {
  expect_equal(center_interval(c("100;100", "0;100", "150;200")), c(100, 50, 175))
  expect_equal(center_interval(c("100;100", "0;Inf", "150;200")), c(100, Inf, 175))
  expect_equal(center_interval(c("100;100", "0;Inf", "150;200"), inf2NA = TRUE), c(100, NA, 175))
  expect_equal(center_interval(c("100;100", "0;100", NA)), c(100, 50, NA))
})
