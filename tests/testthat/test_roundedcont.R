context("rounded cont imputation")
library(testthat)
library(hmi)

set.seed(123)

age <- round(rnorm(150, mean = 55, sd = 10))

tmp <- sample(1:length(age), size = ceiling(length(age)*0.4))#get a 40 % sample
age[tmp] <- 5 * floor(age[tmp]/5 + 0.5)

y_imp <- age


y_imp_low <- age - sample(0:3, size = 150, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.1))
y_imp_up <- age + sample(0:3, size = 150, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.1))
y_imp_roundandinterval <- generate_interval(y_imp_low, y_imp_up)

X_imp <- cbind(1, iris)

imp1 <- imp_roundedcont(y_df = data.frame(y_imp),
                        X_imp = X_imp,
                        PSI = data.frame(cbind(1, iris[, 1])),
                        pvalue = 1)

imp2 <- imp_roundedcont(y_df = data.frame(y_imp_roundandinterval),
                        X_imp = X_imp,
                        PSI = data.frame(cbind(1, iris[, 1])),
                        pvalue = 1,
                        rounding_degrees = c(1, 5, 10))
#test_check("hmi")

test_that("imp_roundedcont returns plausible values", {
  expect_equal(class(imp1), "data.frame")
  expect_equal(class(imp1$y_ret), "numeric")
  expect_equal(sum(is.na(imp1$y_ret)), 0)
  expect_true(all(imp1$y_ret[tmp]  > age[tmp] - 5 & imp1$y_ret[tmp] < age[tmp] + 5))
  expect_equal(class(imp2), "data.frame")
  expect_equal(class(imp2$y_ret), "numeric")
  expect_equal(sum(is.na(imp2$y_ret)), 0)
  expect_true(all(imp2$y_ret >= y_imp_low -5 & imp2$y_ret <= y_imp_up + 5))
})
