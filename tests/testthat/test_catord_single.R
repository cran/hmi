context("cat_single")
library(testthat)
library(hmi)
library(mice)
set.seed(123)

y_impFAC4<- factor(sample(c("A", "B", "C", NA), size = 150, replace = TRUE), ordered = TRUE)
X_imp <- cbind(1, iris[, 1:4])

imp_factor <- imp_orderedcat_single(y_imp = y_impFAC4,
                                    X_imp = X_imp,
                                    pvalue = 1)
#test_check("hmi")

test_that("cat_single returns plausible values", {
  expect_equal(class(imp_factor$y_ret), c("ordered", "factor"))
  expect_equal(sort(as.character(unique(imp_factor$y_ret))), c("A", "B", "C"))
  expect_true(is.ordered(imp_factor$y_ret))
})
