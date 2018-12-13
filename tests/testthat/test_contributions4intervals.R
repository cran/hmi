context("contributions4intervals")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("contributions4intervals works", {
  expect_equal(contributions4intervals(c(-Inf, -Inf, -Inf, -Inf, -Inf), c(Inf, 0, 1, 2, 3), 0, 1),
               pnorm(c(Inf, 0, 1, 2, 3)))
  expect_equal(contributions4intervals(2.7, Inf, 2.7, 3.123), 0.5)
})
