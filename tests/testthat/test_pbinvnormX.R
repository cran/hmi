context("pbivnormX")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("pbivnormX works", {
  expect_equal(pbivnormX(x = 0, y = 0, -0.5), 1/6)
  expect_equal(pbivnormX(x = c(0, 0), y = c(0, Inf), -0.5), c(1/6, 0.5))
  expect_equal(pbivnormX(x = c(0, 0), y = c(0, Inf)), c(0.25, 0.5))
  expect_equal(pbivnormX(matrix(c(0, 0, 0, Inf), 2)), c(0.25, 0.5))
})
