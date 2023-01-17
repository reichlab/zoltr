context("model")

library(httr)
library(jsonlite)
library(mockery)
library(testthat)
library(webmockr)
library(zoltr)
library(lubridate)

test_that("model_info() returns a list", {
  zoltar_connection <- new_connection("http://example.com")
  exp_model_info <- jsonlite::read_json("data/model-1.json")
  exp_model_info$forecasts <- NULL
  exp_model_info$aux_data_url <- NA
  m <- mock(exp_model_info)
  testthat::with_mock("zoltr::get_resource" = m, {
    act_model_info <- model_info(zoltar_connection, "http://example.com/api/model/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/model/1/")
    expect_is(act_model_info, "list")
    expect_equal(act_model_info, exp_model_info)
  })
})


test_that("create_model() creates a Model", {
  zoltar_connection <- new_connection("http://example.com")
  model_info <- jsonlite::read_json("data/model-1.json")
  webmockr::stub_request("post", uri = "http://example.com/api/project/1/models/") %>%
    to_return(
      body = model_info,
      status = 200,
      headers = list('Content-Type' = 'application/json; charset=utf-8'))
  model_config <- jsonlite::read_json("data/example-model-config.json")
  testthat::with_mock("httr::POST" = function(...) {
    called_args <<- list(...)
    load("data/get_token_response.rda")  # 'response' contains a 200 response from a sample `get_token()` call via `zoltar_authenticate()`
    response  # actual response doesn't matter, just its class
  },
                      create_model(zoltar_connection, "http://example.com/api/project/1/", model_config))
  expect_equal(called_args$url, "http://example.com/api/project/1/models/")
  expect_equal(called_args$body$model_config, model_config)
})


test_that("create_model() calls re_authenticate_if_necessary() and returns a Model URL", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  model_info <- jsonlite::read_json("data/model-1.json")
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    webmockr::stub_request("post", uri = "http://example.com/api/project/1/models/") %>%
      to_return(
        body = model_info,
        status = 200,
        headers = list('Content-Type' = 'application/json; charset=utf-8'))
    model_config <- jsonlite::read_json("data/example-model-config.json")
    model_url <- create_model(zoltar_connection, "http://example.com/api/project/1/", model_config)
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(model_url, "http://example.com/api/model/1/")
  })
})


test_that("edit_model() calls re_authenticate_if_necessary()", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  model_info <- jsonlite::read_json("data/model-1.json")
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    webmockr::stub_request("put", uri = "http://example.com/api/model/1/") %>%
      to_return(
        body = model_info,
        status = 200,
        headers = list('Content-Type' = 'application/json; charset=utf-8'))
    model_config <- jsonlite::read_json("data/example-model-config.json")
    edit_model(zoltar_connection, "http://example.com/api/model/1/", model_config)
    expect_equal(length(mock_calls(m)), 1)
  })
})


test_that("edit_model() edits a Model", {
  zoltar_connection <- new_connection("http://example.com")
  model_config <- jsonlite::read_json("data/example-model-config.json")
  testthat::with_mock("httr::PUT" = function(...) {
    called_args <<- list(...)
    load("data/get_token_response.rda")  # 'response' contains a 200 response from a sample `get_token()` call via `zoltar_authenticate()`
    response  # actual response doesn't matter, just its class
  },
                      edit_model(zoltar_connection, "http://example.com/api/model/1/", model_config))
  expect_equal(called_args$url, "http://example.com/api/model/1/")
  expect_equal(called_args$body$model_config, model_config)
})


test_that("delete_model() calls delete_resource", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  testthat::with_mock("zoltr::delete_resource" = m, {
    delete_model(zoltar_connection, "http://example.com/api/model/0/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/model/0/")
  })
})


test_that("forecasts() returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")
  forecasts_list_json <- jsonlite::read_json("data/forecasts-list.json")
  m <- mock(forecasts_list_json)  # return values in calling order
  testthat::with_mock("zoltr::get_resource" = m, {
    the_forecasts <- forecasts(zoltar_connection, "http://example.com/api/model/5/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/model/5/forecasts/")
    expect_is(the_forecasts, "data.frame")
    expect_equal(names(the_forecasts), c("id", "url", "forecast_model_url", "source", "timezero_url", "timezero_date",
                                         "data_version_date", "is_season_start", "created_at", "issued_at", "notes",
                                         "forecast_data_url"))
    expect_equal(nrow(the_forecasts), 2)  # 2 forecasts
    expect_equal(ncol(the_forecasts), 12)

    exp_row <- data.frame(id = 3, url = "http://example.com/api/forecast/3/",
                          forecast_model_url = "http://example.com/api/model/5/",
                          source = "docs-predictions.json",
                          timezero_url = "http://example.com/api/timezero/5/",
                          timezero_date = as.Date("2011-10-02", YYYY_MM_DD_DATE_FORMAT),
                          data_version_date = as.Date(NA),
                          is_season_start = TRUE,
                          created_at = lubridate::parse_date_time(
                            "2020-03-05T15:47:47.369231-05:00", DATE_TIME_TZ_FORMAT, exact = TRUE),
                          issued_at = lubridate::parse_date_time(
                            "2021-05-10T08:38:41.296500-04:00", DATE_TIME_TZ_FORMAT, exact = TRUE),
                          notes = "a small prediction file",
                          forecast_data_url = "http://example.com/api/forecast/3/data/",
                          stringsAsFactors = FALSE)
    forecast_row <- the_forecasts[1,]
    expect_equal(forecast_row, exp_row)
  })
})


test_that("forecasts() can handle null notes", {
  zoltar_connection <- new_connection("http://example.com")
  forecasts_list_json <- jsonlite::read_json("data/forecasts-list-null-notes.json")
  m <- mock(forecasts_list_json)  # return values in calling order
  testthat::with_mock("zoltr::get_resource" = m, {
    the_forecasts <- forecasts(zoltar_connection, "http://example.com/api/model/5/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/model/5/forecasts/")
    expect_is(the_forecasts, "data.frame")
    expect_equal(names(the_forecasts), c("id", "url", "forecast_model_url", "source", "timezero_url", "timezero_date",
                                         "data_version_date", "is_season_start", "created_at", "issued_at", "notes",
                                         "forecast_data_url"))
    expect_equal(nrow(the_forecasts), 2)  # 2 forecasts
    expect_equal(ncol(the_forecasts), 12)

    exp_row <- data.frame(id = 3, url = "http://example.com/api/forecast/3/",
                          forecast_model_url = "http://example.com/api/model/5/",
                          source = "docs-predictions.json",
                          timezero_url = "http://example.com/api/timezero/5/",
                          timezero_date = as.Date("2011-10-02", YYYY_MM_DD_DATE_FORMAT),
                          data_version_date = as.Date(NA),
                          is_season_start = TRUE,
                          created_at = lubridate::parse_date_time(
                            "2020-03-05T15:47:47.369231-05:00", DATE_TIME_TZ_FORMAT, exact = TRUE),
                          issued_at = lubridate::parse_date_time(
                            "2021-05-10T08:38:41.296500-04:00", DATE_TIME_TZ_FORMAT, exact = TRUE),
                          notes = as.character(NA),
                          forecast_data_url = "http://example.com/api/forecast/3/data/",
                          stringsAsFactors = FALSE)
    forecast_row <- the_forecasts[1,]
    expect_equal(forecast_row, exp_row)
  })
})


test_that("upload_forecast() creates a Job", {
  zoltar_connection <- new_connection("http://example.com")
  job_json <- jsonlite::read_json("data/job-2.json")
  mockery::stub(upload_forecast, 'httr::upload_file', NULL)
  webmockr::stub_request("post", uri = "http://example.com/api/model/1/forecasts/") %>%
    to_return(
      body = job_json,
      status = 200,
      headers = list('Content-Type' = 'application/json; charset=utf-8'))

  testthat::with_mock("httr::POST" = function(...) {
    called_args <<- list(...)
    load("data/get_token_response.rda")  # 'response' contains a 200 response from a sample `get_token()` call via `zoltar_authenticate()`
    response  # actual response doesn't matter, just its class
  },
                      upload_forecast(zoltar_connection, "http://example.com/api/model/1/", "2020-02-22", list(), TRUE))
  expect_equal(called_args$url, "http://example.com/api/model/1/forecasts/")
  expect_equal(called_args$body$data_file, NULL)  # due to mockery::stub() calls elsewhere
  expect_equal(called_args$body$timezero_date, "2020-02-22")
})


test_that("upload_forecast() returns a Job URL, and job_info() is correct", {
  zoltar_connection <- new_connection("http://example.com")
  job_json <- jsonlite::read_json("data/job-2.json")
  mockery::stub(upload_forecast, 'httr::upload_file', NULL)
  webmockr::stub_request("post", uri = "http://example.com/api/model/1/forecasts/") %>%
    to_return(
      body = job_json,
      status = 200,
      headers = list('Content-Type' = 'application/json; charset=utf-8'))
  job_url <- upload_forecast(zoltar_connection, "http://example.com/api/model/1/", NULL, list(), TRUE)
  expect_equal(job_url, "http://example.com/api/job/2/")

  # test job_info()
  exp_job_json <- job_json
  exp_job_json$status <- "SUCCESS"
  exp_job_json$created_at <- lubridate::parse_date_time(
    "2019-03-26T14:55:31.028436-04:00", DATE_TIME_TZ_FORMAT, exact = TRUE)
  exp_job_json$updated_at <- lubridate::parse_date_time(
    "2019-03-26T14:55:37.812924-04:00", DATE_TIME_TZ_FORMAT, exact = TRUE)
  exp_job_json$input_json <- list("forecast_model_pk" = 1, "timezero_pk" = 2, notes = "a few predictions")
  exp_job_json$output_json <- list("forecast_pk" = 3)

  m <- mock(job_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    the_job_info <- job_info(zoltar_connection, "http://example.com/api/job/2/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/job/2/")
    expect_is(the_job_info, "list")
    expect_equal(the_job_info, exp_job_json)
  })
})


test_that("upload_forecast() calls re_authenticate_if_necessary()", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    upload_forecast(zoltar_connection, "http://example.com/api/model/1/", NULL, list(), TRUE)
    expect_equal(length(mock_calls(m)), 1)
  })
})


test_that("upload_forecast() passes correct url to POST()", {
  zoltar_connection <- new_connection("http://example.com")
  called_args <- NULL
  timezero_date <- "2019-10-21"
  # Note: this file is a duplicate of vignettes one b/c I could not figure out how to access that directory for both
  # devtools::test() and devtools::check() (different working dirs):
  forecast_data <- jsonlite::read_json("data/docs-predictions.json")
  testthat::with_mock(
    "httr::POST" = function(...) {
      called_args <<- list(...)
      load("data/upload_response.rda")  # 'response' contains 200 response from sample upload_forecast() call
      response
    },
    job_url <- upload_forecast(zoltar_connection, "http://example.com/api/model/1/", timezero_date, forecast_data, TRUE))
  expect_equal(called_args$url, "http://example.com/api/model/1/forecasts/")
  expect_equal(called_args$body$timezero_date, timezero_date)
  expect_s3_class(called_args$body$data_file, "form_file")
})


test_that("upload_forecast() forecast_data/is_json mismatches", {
  zoltar_connection <- new_connection("http://example.com")
  expect_error(upload_forecast(zoltar_connection, "http://example.com/api/model/1/", NULL,
                               data.frame(), TRUE),
               "invalid forecast_data type for is_json", fixed = TRUE)  # `dataframe` but is_json
  expect_error(upload_forecast(zoltar_connection, "http://example.com/api/model/1/", NULL,
                               list(), FALSE),
               "invalid forecast_data type for is_json", fixed = TRUE)  # `list` but not is_json
})


# job_info_forecast_url
test_that("job_info_forecast_url() is correct", {
  zoltar_connection <- new_connection("http://example.com")

  # case 1/2: the_upload_info$output_json DOES have a $forecast_pk
  the_job_info <- list("output_json" = list("forecast_pk" = 3))
  forecast_url <- job_info_forecast_url(zoltar_connection, the_job_info)
  expect_equal(forecast_url, "http://example.com/api/forecast/3/")

  # case 2/2: the_upload_info$output_json does NOT have a $forecast_pk
  the_job_info <- list("output_json" = list("NOT forecast_pk" = 3))
  forecast_url <- job_info_forecast_url(zoltar_connection, the_job_info)
  expect_null(forecast_url)
})
