context("cdc")

library(jsonlite)
library(testthat)
library(zoltr)


test_that("forecast_data_from_cdc_csv_file(cdc_csv_file) is correct", {
  # test internal forecast_data_from_cdc_data_frame()
  expect_error(forecast_data_from_cdc_data_frame(2016, list()), "cdc_data_frame was not a `data.frame`", fixed=TRUE)

  cdc_data_frame <- data.frame(wrong_columns=list(), stringsAsFactors=FALSE)
  expect_error(forecast_data_from_cdc_data_frame(2016, cdc_data_frame), "cdc_data_frame did not have required columns", fixed=TRUE)

  # blue sky
  cdc_csv_file <- "data/20161023-KoTstable-20161109-small.cdc.csv"
  act_forecast_data <- forecast_data_from_cdc_csv_file(2016, cdc_csv_file)
  exp_forecast_data <- jsonlite::read_json("data/20161023-KoTstable-20161109-small-exp-predictions.json")
  expect_is(act_forecast_data, "list")
  expect_equal(names(act_forecast_data), c("predictions"))
  expect_equal(length(act_forecast_data$predictions), 10)
  expect_equal(act_forecast_data, exp_forecast_data)
})
