context("quantile")

library(data.table)
library(jsonlite)
library(testthat)
library(zoltr)


test_that("data_frame_from_forecast_data() is correct", {
  # test the underlying function that `quantile_data_frame_from_forecast_data()` filters from
  cdc_csv_file <- "data/docs-predictions-exp.csv"
  exp_data_frame <- utils::read.csv(cdc_csv_file, stringsAsFactors = FALSE, colClasses='character')  # "NA" -> NA
  exp_data_frame["" == exp_data_frame] <- NA  # "" -> NA
  exp_data_frame <- data.table(exp_data_frame)
  forecast_data <- jsonlite::read_json("data/docs-predictions.json")
  forecast_data <- convert_forecast_data_dates(forecast_data)  # matches what `download_forecast()` does
  act_data_frame <- data_frame_from_forecast_data(forecast_data)
  expect_equal(act_data_frame, exp_data_frame)
})


test_that("data_frame_from_forecast_data() handles retractions", {
  cdc_csv_file <- "data/docs-predictions-with-retractions-exp.csv"
  exp_data_frame <- utils::read.csv(cdc_csv_file, stringsAsFactors = FALSE, colClasses='character')  # "NA" -> NA
  exp_data_frame["" == exp_data_frame] <- NA  # "" -> NA
  exp_data_frame <- data.table(exp_data_frame)
  forecast_data <- jsonlite::read_json("data/docs-predictions-with-retractions.json")
  forecast_data <- convert_forecast_data_dates(forecast_data)  # matches what `download_forecast()` does
  act_data_frame <- data_frame_from_forecast_data(forecast_data)
  expect_equal(act_data_frame, exp_data_frame)
})
