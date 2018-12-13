context("cont_multi")
library(testthat)
library(hmi)

set.seed(123)
y_imp <- iris[, 1]
y_imp[sample(1:length(y_imp), size = 20)] <- NA
X_imp <- cbind(1, iris[, 2:4])
Z_imp1 <- data.frame(rep(1, nrow(iris)))
Z_imp2 <- data.frame(cbind(1, iris[, 2]))
clID <- iris$Species


imp1 <- imp_cont_multi(y_imp = y_imp,
                       X_imp = X_imp,
                       Z_imp = Z_imp1,
                       clID = clID,
                       pvalue = 1)[[1]]

imp2 <- imp_cont_multi(y_imp = y_imp,
                       X_imp = X_imp,
                       Z_imp = Z_imp2,
                       clID = clID,
                       pvalue = 1)[[1]]



#test_check("hmi")

test_that("cont_multi returns plausible values", {
  expect_equal(class(imp1$y_ret), "numeric")
  expect_equal(class(imp2$y_ret), "numeric")
})
