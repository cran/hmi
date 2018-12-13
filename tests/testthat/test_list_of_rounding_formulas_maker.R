context("list_of_rounding_formulas_maker")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("useful rounding formulas are made", {
  expect_equal(list_of_rounding_formulas_maker(data.frame(y = 1, x = c(3, 6, 7, 6, 6, 10, 12, 4, 12, 18, 18, 36, 36))),
               list(x = formula(~.)))
})
