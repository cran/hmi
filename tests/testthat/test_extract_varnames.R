context("extract_varnames")
library(testthat)
library(hmi)
library(lme4)
data("sleepstudy")
foo <- sleepstudy
foo$new <- rnorm(180)
#test_check("hmi")

test_that("correct variable names are extracted", {
  expect_equal(extract_varnames(model_formula = NULL, constant_variables = rep(FALSE, 5), data = iris),
               list(target_varname = NULL,
                    intercept_varname = NULL,
                    fixedeffects_varname = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"),
                    randomeffects_varname = "",
                    interactions = "",
                    clID_varname = ""))
  expect_equal(extract_varnames(model_formula = Sepal.Length ~ ., constant_variables = rep(FALSE, 5), data = iris),
               list(target_varname = "Sepal.Length",
                    intercept_varname = "Intercept",
                    fixedeffects_varname = c("Sepal.Length", "Intercept", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"),
                    randomeffects_varname = "",
                    interactions = "",
                    clID_varname = ""))
  expect_equal(extract_varnames(model_formula = Reaction ~ Days + (Days|Subject), constant_variables = rep(FALSE, 3), data = sleepstudy),
               list(target_varname = "Reaction",
                    intercept_varname = "Intercept",
                    fixedeffects_varname = c("Reaction", "Intercept", "Days"),
                    randomeffects_varname = c("Intercept", "Days"),
                    interactions = "",
                    clID_varname = "Subject"))
  expect_equal(extract_varnames(model_formula = Reaction ~ Days * new + (Days|Subject), constant_variables = rep(FALSE, 4), data = foo),
               list(target_varname = "Reaction",
                    intercept_varname = "Intercept",
                    fixedeffects_varname = c("Reaction", "Intercept", "Days", "new"),
                    randomeffects_varname = c("Intercept", "Days"),
                    interactions = "Days:new",
                    clID_varname = "Subject"))
})
