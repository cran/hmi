context("Mode")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("Mode returns the actual mode", {
  expect_equal(Mode(c(1, 2, 2, 3)), 2)
})
