context("split_interval")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("intervals are split correctly", {
  expect_equal(split_interval(as.interval(c(NA, "2;3"), sna = TRUE)), matrix(c(NA, 2, NA, 3), ncol = 2))
  expect_equal(split_interval(as.interval(c(NA, "2;3"))), matrix(c(-Inf, 2, Inf, 3), ncol = 2))
  expect_equal(split_interval(c("100;100", "Inf;Inf", "150;200")), matrix(c(100, Inf, 150, 100, Inf, 200), ncol = 2))
  expect_equal(split_interval(c("100;100", "Inf;Inf", NA)), matrix(c(100, Inf, NA, 100, Inf, NA), ncol = 2))
})
