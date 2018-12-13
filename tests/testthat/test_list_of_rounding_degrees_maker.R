context("list_of_rounding_degrees_maker")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("list_of_rounding_degrees_maker returns a useful list", {
  expect_equal(list_of_rounding_degrees_maker(data.frame(y = 1, x = c(3, 6, 7, 6, 6, 10, 12, 4, 12, 18, 18, 36, 36))),
               list(x = c(1, 6)))
})
