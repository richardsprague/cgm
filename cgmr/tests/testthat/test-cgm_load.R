# test data loading

library(tidyverse)

test_that("glucose food has data", {
  expect_gt(sum(!is.na(glucose_raw$food)),0)
})
