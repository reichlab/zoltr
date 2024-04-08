context("forecast")

library(httr)
library(jsonlite)
library(testthat)
library(mockery)
library(zoltr)
library(lubridate)

test_that("forecast_info() returns a list", {
  zoltar_connection <- new_connection("http://example.com")

  # test case 1/2: no data_version_date
  forecast_info_json <- jsonlite::read_json("data/forecast-71.json")
  m <- mock(forecast_info_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    exp_forecast_info <- jsonlite::read_json("data/forecast-71.json")
    exp_forecast_info$forecast_model_url <- exp_forecast_info$forecast_model
    exp_forecast_info$forecast_model <- NULL
    exp_forecast_info$created_at <- lubridate::parse_date_time(
      "2020-03-05T15:47:47.369231-05:00", DATE_TIME_TZ_FORMAT, exact=TRUE)
    exp_forecast_info$issued_at <- lubridate::parse_date_time(
      "2021-05-10T08:38:41.296500-04:00", DATE_TIME_TZ_FORMAT, exact=TRUE)
    exp_forecast_info$time_zero$timezero_date <- as.Date("2011-10-02")
    exp_forecast_info$time_zero$data_version_date <- NA
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
    exp_forecast_info$forecast_model_url <- exp_forecast_info$forecast_model
    exp_forecast_info$forecast_model <- NULL
    exp_forecast_info$created_at <- lubridate::parse_date_time(
      "2020-03-05T15:47:47.369231-05:00", DATE_TIME_TZ_FORMAT, exact=TRUE)
    exp_forecast_info$issued_at <- lubridate::parse_date_time(
      "2021-05-10T08:38:41.296500-04:00", DATE_TIME_TZ_FORMAT, exact=TRUE)
    exp_forecast_info$time_zero$timezero_date <- as.Date("2011-10-02")
    exp_forecast_info$time_zero$data_version_date <- as.Date("2011-10-03")
    act_forecast_info <- forecast_info(zoltar_connection, "http://example.com/api/forecast/1/")
    expect_equal(act_forecast_info, exp_forecast_info)
  })
})


test_that("delete_forecast() passes correct URL and returns a Job URL", {
  zoltar_connection <- new_connection("http://example.com")
  load("data/delete_response.rda")  # 'response' contains 200 JSON response from sample 'DELETE' call
  m <- mock(response)
  testthat::with_mock("httr::DELETE" = m, {
    job_url <- delete_forecast(zoltar_connection, "http://example.com/api/forecast/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[1]], "http://example.com/api/forecast/1/")
    expect_equal(job_url, "http://127.0.0.1:8000/api/job/36/")

    # test job_info()
    job_json <- jsonlite::read_json("data/job-2.json")
    m <- mock(job_json)
    testthat::with_mock("zoltr::get_resource" = m, {
      the_job_info <- job_info(zoltar_connection, "http://example.com/api/job/2/")
      expect_equal(length(mock_calls(m)), 1)
      expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/job/2/")
    })
  })
})


test_that("download_forecast() returns JSON data as a list", {
  zoltar_connection <- new_connection("http://example.com")
  # Note: this file is a duplicate of vignettes one b/c I could not figure out how to access that directory for both
  # devtools::test() and devtools::check() (different working dirs):
  predictions_list_json <- jsonlite::read_json("data/docs-predictions.json")
  m <- mock(predictions_list_json)  # return values in calling order
  testthat::with_mock("zoltr::get_resource" = m, {
    act_data <- download_forecast(zoltar_connection, "http://example.com/api/forecast/1/")
    expect_is(act_data, "list")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/forecast/1/data/")

    expect_equal(length(act_data$predictions), 32)

    # spot-check a few "Season peak week" predictions (a date target)
    prediction_element <- act_data$predictions[[1]]
    expect_equal(prediction_element$unit, "loc1")
    expect_equal(prediction_element$target, "Season peak week")
    expect_equal(prediction_element$class, "point")
    expect_equal(prediction_element$prediction$value, as.Date("2019-12-22", YYYY_MM_DD_DATE_FORMAT))

    prediction_element <- act_data$predictions[[14]]
    expect_equal(prediction_element$unit, "loc2")
    expect_equal(prediction_element$target, "Season peak week")
    expect_equal(prediction_element$class, "bin")
    expect_equal(prediction_element$prediction$cat[[1]], as.Date("2019-12-15", YYYY_MM_DD_DATE_FORMAT))

    prediction_element <- act_data$predictions[[15]]
    expect_equal(prediction_element$unit, "loc2")
    expect_equal(prediction_element$target, "Season peak week")
    expect_equal(prediction_element$class, "quantile")
    expect_equal(prediction_element$prediction$value[[1]], as.Date("2019-12-22", YYYY_MM_DD_DATE_FORMAT))

    prediction_element <- act_data$predictions[[26]]
    expect_equal(prediction_element$unit, "loc3")
    expect_equal(prediction_element$target, "Season peak week")
    expect_equal(prediction_element$class, "sample")
    expect_equal(prediction_element$prediction$sample[[1]], as.Date("2020-01-06", YYYY_MM_DD_DATE_FORMAT))
  })
})
