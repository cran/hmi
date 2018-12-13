context("imputationcycle")
library(testthat)
library(hmi)
library(mice)

set.seed(123)
original_data <- iris
original_data[sample(1:150, size = 20), 1] <- NA
data_before <- original_data
data_before <- cbind(1, original_data)
NA_locator <- is.na(data_before)
model_formula <- formula(Sepal.Length ~ .)
constant_variables <- rep(FALSE, 5)
fe <- extract_varnames(model_formula = model_formula,
                       constant_variables = constant_variables,
                       variable_names_in_data = colnames(data_before),
                       data = data_before)
interaction_names <- NULL
list_of_types <- list_of_types_maker(original_data)

tmp1 <- imputationcycle(data_before = data_before,
                       original_data = original_data,
                       NA_locator = NA_locator,
                       fe = fe,
                       interaction_names = interaction_names,
                       list_of_types = list_of_types,
                       nitt = 22000,
                       burnin = 2000,
                       thin = 1,
                       pvalue = 0.2,
                       mn = 1,
                       k = Inf,
                       spike = NULL,
                       rounding_degrees = NULL,
                       rounding_covariates = NULL)

original_data2 <- original_data
for(j in 1:ncol(original_data2)){
  original_data2[sample(1:150, size = 20), j] <- NA
}
data_before2 <- cbind(1, original_data2)
NA_locator2 <- is.na(data_before2)

tmp2 <- imputationcycle(data_before = data_before2,
                        original_data = original_data2,
                        NA_locator = NA_locator2,
                        fe = fe,
                        interaction_names = interaction_names,
                        list_of_types = list_of_types,
                        nitt = 22000,
                        burnin = 2000,
                        thin = 1,
                        pvalue = 0.2,
                        mn = 1,
                        k = Inf,
                        spike = NULL,
                        rounding_degrees = NULL,
                        rounding_covariates = NULL)

model_formula3 <- formula(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width +  (1|Species))
fe3 <- extract_varnames(model_formula = model_formula3,
                       constant_variables = constant_variables,
                       variable_names_in_data = colnames(data_before),
                       data = data_before)

tmp3 <- imputationcycle(data_before = data_before,
                        original_data = original_data,
                        NA_locator = NA_locator,
                        fe = fe3,
                        interaction_names = interaction_names,
                        list_of_types = list_of_types,
                        nitt = 22000,
                        burnin = 2000,
                        thin = 1,
                        pvalue = 0.2,
                        mn = 1,
                        k = Inf,
                        spike = NULL,
                        rounding_degrees = NULL,
                        rounding_covariates = NULL)
#test_check("hmi")

test_that("imputationcycle returns plausible values", {
  expect_equal(sum(is.na(tmp1$data_after)), 0)
  expect_equal(sum(is.na(tmp2$data_after)), 0)
  expect_equal(sum(is.na(tmp3$data_after)), 0)
})
