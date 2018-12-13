context("list_of_spikes_maker")
library(testthat)
library(hmi)

#test_check("hmi")

test_that("list_of_spikes_maker works", {
  expect_equivalent(list_of_spikes_maker(iris), list(Sepal.Width = 3, Peta.Width = 0.2))
  #replace expect_equivalent with expect_equal, after names bug is gone (https://stat.ethz.ch/pipermail/r-devel/2018-August/076630.html).
})
