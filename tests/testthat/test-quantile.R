context("quantile")

library(jsonlite)
library(testthat)
library(zoltr)


test_that("quantile_data_frame_from_forecast_data() is correct", {
  cdc_csv_file <- "data/docs-predictions-quantile-exp.csv"
  exp_data_frame <- utils::read.csv(cdc_csv_file, stringsAsFactors = FALSE, colClasses='character')  # "NA" -> NA
  exp_data_frame["" == exp_data_frame] <- NA  # "" -> NA
  exp_data_frame <- data.table(exp_data_frame)
  forecast_data <- jsonlite::read_json("data/docs-predictions.json")
  forecast_data <- convert_forecast_data_dates(forecast_data)  # matches what `download_forecast()` does
  act_data_frame <- quantile_data_frame_from_forecast_data(forecast_data)
  expect_equal(as.data.table(act_data_frame), as.data.table(exp_data_frame))
})
