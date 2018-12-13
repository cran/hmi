context("decompose_interval")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("intervals are decomposed correctly", {
  expect_equal(decompose_interval(c("1;1", "2;3")),
               matrix(c(1, NA, NA, 2, NA, 3, 1, 2, 1, 3), ncol = 5,
                      dimnames = list(NULL, c("precise", "lower_imprecise", "upper_imprecise", "lower_general", "upper_general"))))
  expect_equal(decompose_interval(c("100;100", "Inf;Inf", "150;200")),
               matrix(c(100, NA, NA, NA, Inf, 150, NA, Inf, 200, 100, Inf, 150, 100, Inf, 200), ncol = 5,
                      dimnames = list(NULL, c("precise", "lower_imprecise", "upper_imprecise", "lower_general", "upper_general"))))
  expect_equal(decompose_interval(c("100;100", "Inf;Inf", NA)),
               matrix(c(100, NA, NA, NA, Inf, -Inf, NA, Inf, Inf, 100, Inf, -Inf, 100, Inf, Inf), ncol = 5,
                      dimnames = list(NULL, c("precise", "lower_imprecise", "upper_imprecise", "lower_general", "upper_general"))))
  expect_equal(decompose_interval(c("100;100", "Inf;Inf", NA, "0;1", "-Inf;Inf")),
               matrix(c(100, NA, NA, NA, NA,
                        NA, Inf, -Inf, 0, -Inf,
                        NA, Inf, Inf, 1, Inf,
                        100, Inf, -Inf, 0, -Inf,
                        100, Inf, Inf, 1, Inf), ncol = 5,
                      dimnames = list(NULL, c("precise", "lower_imprecise", "upper_imprecise", "lower_general", "upper_general"))))
})
