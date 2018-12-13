context("interval imputation")
library(testthat)
library(hmi)

set.seed(123)
y_imp_low <- ifelse(runif(150) > 0.8, NA, rpois(150, lambda = 5))
y_imp_up <- rpois(150, lambda = 25)

y_imp <- generate_interval(y_imp_low, y_imp_up)
X_imp <- cbind(1, iris)

imp1 <- imp_interval(y_imp = y_imp,
                     X_imp = X_imp,
                     pvalue = 1)
#test_check("hmi")

test_that("imp_interval returns plausible values", {
  expect_equal(class(imp1), "data.frame")
  expect_equal(class(imp1$y_ret), "numeric")
  expect_equal(sum(imp1$y_ret < y_imp_low, na.rm = TRUE), 0)
  expect_equal(sum(imp1$y_ret > y_imp_up, na.rm = TRUE), 0)
})
