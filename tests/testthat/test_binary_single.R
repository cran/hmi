context("binary_single")
library(testthat)
library(hmi)
library(mice)

set.seed(123)
y_imp <- sample(c(0, 1, NA), size = 150, replace = TRUE)
y_imp2 <- sample(c("A", "B", NA), size = 150, replace = TRUE)
X_imp <- cbind(1, iris[, 1:4])
#test_check("hmi")

test_that("binary_single returns plausible values", {
  expect_equal(unique(imp_binary_single(y_imp = y_imp,
                   X_imp = X_imp,
                   pvalue = 1)$y_ret), c(0, 1))
  expect_equal(sort(as.character(unique(imp_binary_single(y_imp = y_imp2,
                                        X_imp = X_imp,
                                        pvalue = 1)$y_ret))), c("A", "B"))
})
