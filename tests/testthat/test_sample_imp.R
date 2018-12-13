context("sample_imp")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("sample_imp gives useful output", {
  expect_equal(sample_imp(1:10)[,1], 1:10)
  expect_true(sample_imp(c(1:10, NA))[11, 1] %in% 1:10)
})
