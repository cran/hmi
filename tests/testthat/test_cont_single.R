context("cont_single")
library(testthat)
library(hmi)

set.seed(123)
y_imp <- iris[, 1]
y_imp[sample(1:length(y_imp), size = 20)] <- NA
X_imp <- cbind(1, iris[, 2:4])


imp1 <- imp_cont_single(y_imp = y_imp,
                        X_imp = X_imp,
                        pvalue = 1)




#test_check("hmi")

test_that("cont_single returns plausible values", {
  expect_equal(class(imp1$y_ret), "numeric")
})
