context("cat_multi")
library(testthat)
library(hmi)

set.seed(123)
y_imp <- ifelse(runif(150)>0.8, NA, rpois(150, lambda = 5))
X_imp <- cbind(1, iris[, 1:4])
Z_imp1 <- data.frame(rep(1, nrow(iris)))
Z_imp2 <- data.frame(cbind(1, iris[, 1]))

clID <- iris$Species

imp1 <- imp_count_multi(y_imp = y_imp,
                        X_imp = X_imp,
                        Z_imp = Z_imp1,
                        clID = clID,
                        pvalue = 1)



imp2 <- imp_count_multi(y_imp = y_imp,
                        X_imp = X_imp,
                        Z_imp = Z_imp2,
                        clID = clID,
                        pvalue = 1)



#test_check("hmi")

test_that("count_multi returns plausible values", {
  expect_equal(class(imp1), "list")
  expect_equal(class(imp1$y_ret), "data.frame")
  expect_equal(class(imp1$y_ret$y_ret), "integer")
  expect_equal(class(imp2$y_ret$y_ret), "integer")
})
