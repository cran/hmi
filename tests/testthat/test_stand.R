context("stand")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("data are standardized correctly", {
  expect_equal(stand(data.frame(x = 2:4)), data.frame(x = -1:1))
  expect_equal(stand(iris[, 1, drop = FALSE])[, 1], (iris[, 1] - mean(iris[, 1]))/sd(iris[, 1]))
})
