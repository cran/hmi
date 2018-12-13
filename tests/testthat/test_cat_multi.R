context("cat_multi")
library(testthat)
library(hmi)

set.seed(123)
y_impCHAR3 <- sample(c("A", "B", "C", NA), size = 150, replace = TRUE)
y_impFAC4 <- factor(sample(c("A", "B", "C", "D", NA), size = 150, replace = TRUE))
X_imp <- cbind(1, iris[, 1:4])
Z_imp1 <- data.frame(rep(1, nrow(iris)))
Z_imp2 <- data.frame(cbind(1, iris[, 1]))

clID <- iris$Species

imp_character <- imp_cat_multi(y_imp = y_impCHAR3,
                               X_imp = X_imp,
                               Z_imp = Z_imp1,
                               clID = clID,
                               pvalue = 1)[[1]]



imp_factor1 <- imp_cat_multi(y_imp = y_impFAC4,
                             X_imp = X_imp,
                             Z_imp = Z_imp1,
                             clID = clID,
                             pvalue = 1)[[1]]

imp_factor2 <- imp_cat_multi(y_imp = y_impFAC4,
                             X_imp = X_imp,
                             Z_imp = Z_imp2,
                             clID = clID,
                             pvalue = 1)[[1]]



#test_check("hmi")

test_that("cat_multi returns plausible values", {
  expect_equal(class(imp_character$y_ret), "character")
  expect_equal(sort(as.character(unique(imp_character$y_ret))), c("A", "B", "C"))
  expect_equal(class(imp_factor1$y_ret), "factor")
  expect_equal(sort(as.character(unique(imp_factor1$y_ret))), c("A", "B", "C", "D"))
  expect_equal(levels(imp_factor1$y_ret), c("A", "B", "C", "D"))
  expect_equal(class(imp_factor2$y_ret), "factor")
  expect_equal(sort(as.character(unique(imp_factor2$y_ret))), c("A", "B", "C", "D"))
  expect_equal(levels(imp_factor2$y_ret), c("A", "B", "C", "D"))
})
