context("hmi_pool")
library(testthat)
library(hmi)
library(lme4)
test_analysis_function <- function(complete_data){
  parameters_of_interest <- list()

  my_model <- lm(ind310 ~ 0 + ., data = complete_data)
  return(coef(my_model))
}
#remove class "labelled" as it will produce warnings in combination with mice and dplyr
for(i in 1:9) class(nhanes_imp$data[, i]) <- "integer"
tmp <- hmi_pool(nhanes_imp, test_analysis_function)
#test_check("hmi")

test_that("pooling with hmi works", {
  expect_equal(class(tmp),
               "numeric")
  expect_equal(sum(is.na(tmp)), 0)
  expect_equal(names(tmp), c("inq020", "inq012", "inq030", "inq060", "inq080", "inq090",
                             "inq132", "inq140", "inq150", "ind235", "inq320In my car", "inq320In a car that belongs to someone I live with",
                             "inq320In a car that belongs to someone who lives elsewhere",
                             "inq320Walk", "inq320Ride bicycle", "inq320Bus, subway or other public transit",
                             "inq320Taxi or other paid driver", "inq320Someone else delivers groceries",
                             "inq320Other", "inq320No usual mode of traveling to store"))
})
