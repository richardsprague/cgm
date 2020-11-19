library(tidyverse)
library(lubridate)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("AUC basics work", {
  expect_equal(lubridate::as_date(today_is()),lubridate::today())
})

test_that("AUC calculation works",{
  expect_equal(auc_calc(glucose_raw) %>% as.character(), "201.916666666667")
})

test_that("glucose dataframe is correct",{
  expect_equal(glucose_raw$value[1], 131)
})

test_that("Food effect works",{
  expect_equal(food_effect(activity_df = activity_raw,glucose_df = glucose_raw)[3,"hist"], 83)
})

test_that("Food AUC works",{
  expect_equal(food_auc("Latte",activity_df=activity_raw,glucose_df=glucose_raw)$auc[3] %>% as.integer(),169)
})