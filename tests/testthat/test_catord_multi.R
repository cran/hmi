context("catord_multi")
library(testthat)
library(hmi)

set.seed(123)
y_impFAC4 <- factor(sample(c("A", "B", "C", "D", NA), size = 150, replace = TRUE), ordered = TRUE)
X_imp <- cbind(1, iris[, 1:4])
Z_imp1 <- data.frame(rep(1, nrow(iris)))
Z_imp2 <- data.frame(cbind(1, iris[, 1]))

clID <- iris$Species


imp_factor1 <- imp_orderedcat_multi(y_imp = y_impFAC4,
                             X_imp = X_imp,
                             Z_imp = Z_imp1,
                             clID = clID,
                             pvalue = 1)[[1]]

imp_factor2 <- imp_orderedcat_multi(y_imp = y_impFAC4,
                             X_imp = X_imp,
                             Z_imp = Z_imp2,
                             clID = clID,
                             pvalue = 1)[[1]]



#test_check("hmi")

test_that("catord_multi returns plausible values", {
  expect_equal(class(imp_factor1$y_ret), c("ordered", "factor"))
  expect_equal(sort(as.character(unique(imp_factor1$y_ret))), c("A", "B", "C", "D"))
  expect_equal(levels(imp_factor1$y_ret), c("A", "B", "C", "D"))
  expect_equal(class(imp_factor2$y_ret), c("ordered", "factor"))
  expect_equal(sort(as.character(unique(imp_factor2$y_ret))), c("A", "B", "C", "D"))
  expect_equal(levels(imp_factor2$y_ret), c("A", "B", "C", "D"))
  expect_true(is.ordered(imp_factor1$y_ret))
  expect_true(is.ordered(imp_factor2$y_ret))
})
