context("list_of_types_maker")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("list_of_types_maker returns the correct list", {
  expect_equal(list_of_types_maker(iris),
               list(Sepal.Length = "cont", Sepal.Width = "semicont", Petal.Length = "cont", Petal.Width = "semicont", Species = "categorical"))
})
