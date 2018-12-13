context("sna_interval")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("-Inf;Inf is changed to NA", {
  expect_equal(sna_interval(c("100;100", "0;100", "150;200")), c("100;100", "0;100", "150;200"))
  expect_equal(sna_interval(c("100;100", "Inf;Inf", "150;200")), c("100;100", "Inf;Inf", "150;200"))
  expect_equal(sna_interval(c("100;100", "-Inf;Inf", "150;200")), c("100;100", NA, "150;200"))
  expect_equal(sna_interval(c("100;100", NA, "150;200")), c("100;100", NA, "150;200"))
})
