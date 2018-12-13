context("binary_multi")
library(testthat)
library(hmi)

set.seed(123)
y_imp <- sample(c(0, 1, NA), size = 150, replace = TRUE)
y_imp2 <- sample(c("A", "B", NA), size = 150, replace = TRUE)
X_imp <- cbind(1, iris[, 1:4])
Z_imp <- data.frame(cbind(1, iris[, 1]))
clID <- iris$Species
#test_check("hmi")

test_that("binary_multi returns plausible values", {
  expect_equal(unique(imp_binary_multi(y_imp = y_imp,
                   X_imp = X_imp,
                   Z_imp = Z_imp,
                   clID = clID,
                   pvalue = 1)[[1]]$y_ret), c(0, 1))
  expect_equal(sort(as.character(unique(imp_binary_multi(y_imp = y_imp2,
                                       X_imp = X_imp,
                                       Z_imp = Z_imp,
                                       clID = clID,
                                       pvalue = 1)[[1]]$y_ret))), c("A", "B"))
})
