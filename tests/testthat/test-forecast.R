context("forecast")

library(httr)
library(jsonlite)
library(testthat)
library(zoltr)


test_that("forecast_info() returns a list", {
  zoltar_connection <- new_connection("http://example.com")

  # test case 1/2: no data_version_date
  forecast_info_json <- jsonlite::read_json("data/forecast-71.json")
  m <- mock(forecast_info_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    exp_forecast_info <- jsonlite::read_json("data/forecast-71.json")
    exp_forecast_info$created_at <- as.Date("2020-03-05T15:47:47.369231-05:00")
    exp_forecast_info$time_zero$timezero_date <- as.Date("2011-10-02")
    exp_forecast_info$time_zero$data_version_date <- as.Date(NA)
    act_forecast_info <- forecast_info(zoltar_connection, "http://example.com/api/forecast/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/forecast/1/")
    expect_is(act_forecast_info, "list")
    expect_equal(act_forecast_info, exp_forecast_info)
  })

  # test case 2/2: yes data_version_date
  forecast_info_json <- jsonlite::read_json("data/forecast-71.json")
  forecast_info_json$time_zero$data_version_date <- as.Date("2011-10-03")
  m <- mock(forecast_info_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    exp_forecast_info <- jsonlite::read_json("data/forecast-71.json")
    exp_forecast_info$created_at <- as.Date("2020-03-05T15:47:47.369231-05:00")
    exp_forecast_info$time_zero$timezero_date <- as.Date("2011-10-02")
    exp_forecast_info$time_zero$data_version_date <- as.Date("2011-10-03")
    act_forecast_info <- forecast_info(zoltar_connection, "http://example.com/api/forecast/1/")
    expect_equal(act_forecast_info, exp_forecast_info)
  })
})


test_that("delete_forecast() passes correct URL", {
  zoltar_connection <- new_connection("http://example.com")
  load("data/delete_response.rda")  # 'delete_response' contains 204 response from sample 'DELETE' call
  m <- mock(delete_response)
  testthat::with_mock("httr::DELETE" = m, {
    delete_forecast(zoltar_connection, "http://example.com/api/forecast/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[1]], "http://example.com/api/forecast/1/")
  })
})


test_that("download_forecast() returns JSON data as a list", {
  zoltar_connection <- new_connection("http://example.com")
  predictions_list_json <- jsonlite::read_json("data/docs-predictions.json")
  m <- mock(predictions_list_json)  # return values in calling order
  testthat::with_mock("zoltr::get_resource" = m, {
    act_data <- download_forecast(zoltar_connection, "http://example.com/api/forecast/1/")
    expect_is(act_data, "list")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/forecast/1/data/")

    expect_equal(length(act_data$predictions), 26)

    # spot-check a few "Season peak week" predictions (a date target)
    prediction_element <- act_data$predictions[[1]]
    expect_equal(prediction_element$unit, "location1")
    expect_equal(prediction_element$target, "Season peak week")
    expect_equal(prediction_element$class, "point")
    expect_equal(prediction_element$prediction$value, as.Date("2019-12-22", YYYY_MM_DD_DATE_FORMAT))

    prediction_element <- act_data$predictions[[11]]
    expect_equal(prediction_element$unit, "location2")
    expect_equal(prediction_element$target, "Season peak week")
    expect_equal(prediction_element$class, "bin")
    expect_equal(prediction_element$prediction$cat[[1]], as.Date("2019-12-15", YYYY_MM_DD_DATE_FORMAT))

    prediction_element <- act_data$predictions[[21]]
    expect_equal(prediction_element$unit, "location3")
    expect_equal(prediction_element$target, "Season peak week")
    expect_equal(prediction_element$class, "sample")
    expect_equal(prediction_element$prediction$sample[[1]], as.Date("2020-01-06", YYYY_MM_DD_DATE_FORMAT))

  })
})