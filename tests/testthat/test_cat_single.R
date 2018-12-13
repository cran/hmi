context("cat_single")
library(testthat)
library(hmi)

set.seed(123)
y_imp <- sample(c("A", "B", "C", NA), size = 150, replace = TRUE)
y_imp2 <- factor(sample(c("A", "B", "C", NA), size = 150, replace = TRUE))
X_imp <- cbind(1, iris[, 1:4])
imp_character <- imp_cat_single(y_imp = y_imp,
                                X_imp = X_imp,
                                pvalue = 1)
imp_factor <- imp_cat_single(y_imp = y_imp2,
                          X_imp = X_imp,
                          pvalue = 1)
#test_check("hmi")

test_that("cat_single returns plausible values", {
  expect_equal(sort(as.character(unique(imp_character$y_ret))), c("A", "B", "C"))
  expect_equal(class(imp_factor$y_ret), "factor")
  expect_equal(sort(as.character(unique(imp_factor$y_ret))), c("A", "B", "C"))
  expect_equal(levels(imp_factor$y_ret), c("A", "B", "C"))
})
