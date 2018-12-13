context("count_single")
library(testthat)
library(hmi)

set.seed(123)
y_imp <- ifelse(runif(150) > 0.8, NA, rpois(150, lambda = 5))
X_imp <- cbind(1, iris)

imp1 <- imp_count_single(y_imp = y_imp,
                         X_imp = X_imp,
                         pvalue = 1)
#test_check("hmi")

test_that("count_single returns plausible values", {
  expect_equal(class(imp1), "list")
  expect_equal(class(imp1$y_ret), "data.frame")
  expect_equal(class(imp1$y_ret$y_ret), "integer")
})
