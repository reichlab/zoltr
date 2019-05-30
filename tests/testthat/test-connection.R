context("connection")

library(jsonlite)
library(httr)
library(mockery)
library(testthat)
library(zoltr)
library(webmockr)
library(base64url)

httr_mock()  # turns on for *all* tests


#
# ---- utilities ----
#
# NB: these assume that this file is loaded in order, i.e., that they are called before any tests
#

two_projects_json <- jsonlite::read_json("projects-list.json")

# mock_token is an expired token as returned by zoltar. decoded contents:
# - header:  {"typ": "JWT", "alg": "HS256"}
# - payload: {"user_id": 3, "username": "model_owner1", "exp": 1558442805, "email": ""}
# - expiration:
#   05/21/2019 @ 12:46pm               (UTC)
#   2019-05-21T12:46:45+00:00          (ISO 8601)
#   Tuesday, May 21, 2019 12:46:45 PM  (GMT)
#   datetime(2019, 5, 21, 12, 46, 45)  (python): datetime.utcfromtimestamp(1558442805)
mock_token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyX2lkIjozLCJ1c2VybmFtZSI6Im1vZGVsX293bmVyMSIsImV4cCI6MTU1ODQ0MjgwNSwiZW1haWwiOiIifQ.o03V2RxkFpA5ThhRAidwDWCdcQNeJzr1wwFkOFKUI74"

mock_authenticate <- function(zoltar_connection, token=mock_token) {
  with_mock(
    "zoltr::get_token" = function(...) {
      token
    },
    zoltar_authenticate(zoltar_connection, "username", "password"))
}


#
# ---- utility tests ----
#

test_that("id_for_url(url) returns an integer", {
  expect_equal(id_for_url("http://example.com/api/forecast/1/"), 1L)
  expect_equal(id_for_url("http://example.com/api/forecast/1"), 1L)
})


test_that("url_for_project_id(zoltar_connection, project_id) returns a URL", {
  zoltar_connection <- new_connection("http://example.com")
  expect_equal(url_for_project_id(zoltar_connection, 1L), "http://example.com/api/project/1")
})


test_that("url_for_model_id(zoltar_connection, project_id) returns a URL", {
  zoltar_connection <- new_connection("http://example.com")
  expect_equal(url_for_model_id(zoltar_connection, 1L), "http://example.com/api/model/1")
})


test_that("url_for_model_forecasts_id(zoltar_connection, project_id) returns a URL", {
  zoltar_connection <- new_connection("http://example.com")
  expect_equal(url_for_model_forecasts_id(zoltar_connection, 1L), "http://example.com/api/model/1/forecasts/")
})


test_that("url_for_upload_file_job_id(zoltar_connection, upload_file_job_id) returns a URL", {
  zoltar_connection <- new_connection("http://example.com")
  expect_equal(url_for_upload_file_job_id(zoltar_connection, 1L), "http://example.com/api/uploadfilejob/1")
})


test_that("url_for_forecast_id(zoltar_connection, forecast_id) returns a URL", {
  zoltar_connection <- new_connection("http://example.com")
  expect_equal(url_for_forecast_id(zoltar_connection, 1L), "http://example.com/api/forecast/1")
})


test_that("url_for_forecast_data_id(zoltar_connection, forecast_id) returns a URL", {
  zoltar_connection <- new_connection("http://example.com")
  expect_equal(url_for_forecast_data_id(zoltar_connection, 1L), "http://example.com/api/forecast/1/data/")
})


test_that("url_for_token_auth(zoltar_connection) returns a URL", {
  zoltar_connection <- new_connection("http://example.com")
  expect_equal(url_for_token_auth(zoltar_connection), "http://example.com/api-token-auth/")
})


#
# ---- connection tests ----
#

test_that("new_connection() returns a ZoltarConnection object", {
  zoltar_connection <- new_connection("http://example.com")
  expect_is(zoltar_connection, "ZoltarConnection")
})


test_that("zoltar_authenticate() saves username, password, and session", {
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)

  expect_equal(zoltar_connection$username, "username")
  expect_equal(zoltar_connection$password, "password")
  expect_is(zoltar_connection$session, "ZoltarSession")
  expect_equal(zoltar_connection$session$token, mock_token)
})


test_that("is_token_expired() works for an expired and unexpired tokens", {
  # test an expired token
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)  # default token (mock_token) is expired
  expect_true(is_token_expired(zoltar_connection$session))

  # construct and test an unexpired token
  token_split <- strsplit(mock_token, ".", fixed=TRUE)  # 3 parts: header, payload, and signature
  old_header <- token_split[[1]][[1]]
  old_signature <- token_split[[1]][[3]]

  ten_min_from_now <- round(unclass(Sys.time() + (60 * 10)))  # exclude decimal portion - throws off some JWT tools
  new_payload <- list(user_id=3, username="model_owner1", exp=ten_min_from_now, email="")
  new_payload_json <- jsonlite::toJSON(new_payload, auto_unbox=TRUE)
  unexpired_token <- paste0(old_header, '.', base64url::base64_urlencode(new_payload_json), '.', old_signature)

  mock_authenticate(zoltar_connection, token=unexpired_token)
  expect_false(is_token_expired(zoltar_connection$session))
})


test_that("httr functions re-authenticate expired tokens", {
  # test that all httr methods re-authenticate. the current functions that call httr methods follow.
  # todo update this when adding new functionality
  # - GET     <- get_resource()
  # - DELETE  <- delete_resource()
  # - POST    <- upload_forecast() , get_token()

  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)  # default token (mock_token) is expired

  webmockr::stub_request('get', uri='http://example.com')
  webmockr::stub_request('delete', uri='http://example.com/api/forecast/0')
  webmockr::stub_request('post', uri='http://example.com/api/model/0/forecasts/')
  webmockr::stub_request('post', uri = 'http://example.com/api-token-auth/')
  mockery::stub(upload_forecast, 'httr::upload_file', NULL)

  m <- mock()
  testthat::with_mock("zoltr::zoltar_authenticate" = m, {
    get_resource(zoltar_connection, "http://example.com")  # httr::GET
    expect_equal(length(mock_calls(m)), 1)
  })

  m <- mock()
  testthat::with_mock("zoltr::zoltar_authenticate" = m, {
    delete_forecast(zoltar_connection, forecast_id=0L)  # httr::DELETE
    expect_equal(length(mock_calls(m)), 1)
  })

  m <- mock()
  testthat::with_mock("zoltr::zoltar_authenticate" = m, {
    upload_forecast(zoltar_connection, 0L, NULL, "")  # httr::POST. model_id, timezero_date, forecast_csv_file
    expect_equal(length(mock_calls(m)), 1)
  })

  m <- mock()
  testthat::with_mock("zoltr::zoltar_authenticate" = m, {
    get_token(zoltar_connection$session)  # httr::POST
    expect_equal(length(mock_calls(m)), 0)  # ensure get_token() does *not* call zoltar_authenticate() - o/w inf loop!
  })
})


test_that("upload_forecast() does not call add_headers() for unauthenticated connection", {
  zoltar_connection <- new_connection("http://example.com")  # unauthenticated
  mockery::stub(upload_forecast, 'httr::upload_file', NULL)
  webmockr::stub_request('post', uri='http://example.com/api/model/0/forecasts/')
  m <- mock()
  testthat::with_mock("httr::add_headers" = m, {
    upload_forecast(zoltar_connection, 0L, NULL, "")  # model_id, timezero_date, forecast_csv_file
    expect_equal(length(mock_args(m)), 0)
  })
})


test_that("delete_forecast() does not call add_headers() for unauthenticated connection", {
  zoltar_connection <- new_connection("http://example.com")  # unauthenticated
  webmockr::stub_request('delete', uri='http://example.com/api/forecast/0')
  m <- mock()
  testthat::with_mock("httr::add_headers" = m, {
    delete_forecast(zoltar_connection, forecast_id=0L)
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(names(mock_args(m)[[1]]), "Content-Type")  # httr::DELETE() adds
  })
})


test_that("get_resource() does not call add_headers() for unauthenticated connection", {
  zoltar_connection <- new_connection("http://example.com")  # unauthenticated
  webmockr::stub_request('get', uri='http://example.com')
  m <- mock()
  testthat::with_mock("httr::add_headers" = m, {
    get_resource(zoltar_connection, "http://example.com")
    expect_equal(length(mock_calls(m)), 0)
  })
})


test_that("scores() does not call add_headers() for unauthenticated connection", {
  zoltar_connection <- new_connection("http://example.com")  # unauthenticated
  webmockr::stub_request('get', uri='http://example.com/api/project/0/score_data/')
  m <- mock()
  testthat::with_mock("httr::add_headers" = m, {
    scores(zoltar_connection, 0L)
    expect_equal(length(mock_calls(m)), 0)
  })
})


test_that("forecast_data() does not call add_headers() for unauthenticated connection", {
  zoltar_connection <- new_connection("http://example.com")  # unauthenticated
  webmockr::stub_request('get', uri='http://example.com/api/forecast/0/data/?format=csv')
  m <- mock()
  testthat::with_mock("httr::add_headers" = m, {
    forecast_data(zoltar_connection, 0L, is_json=FALSE)  # CSV
    expect_equal(length(mock_calls(m)), 0)
  })
})


#
# ---- project tests ----
#

test_that("projects(zoltar_connection) returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock(two_projects_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    the_projects <- projects(zoltar_connection)
    expect_is(the_projects, "data.frame")
    expect_equal(nrow(the_projects), 2)  # 2 projects
    expect_equal(ncol(the_projects), 8)
    expect_equal(names(the_projects),
    c("id", "url", "owner_id", "public", "name", "description", "home_url", "core_data"))
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/projects/")

    project_row <- the_projects[1, ]
    exp_row <- data.frame(id=1L, url="http://example.com/api/project/1/", name="public project", stringsAsFactors=FALSE)
    expect_equal(project_row$id, exp_row$id)
    expect_equal(project_row$url, exp_row$url)
    expect_equal(project_row$name, exp_row$name)

    project_row <- the_projects[2, ]
    exp_row <- data.frame(id=2L, url="http://example.com/api/project/2/", name="private project", stringsAsFactors=FALSE)
    expect_equal(project_row$id, exp_row$id)
    expect_equal(project_row$url, exp_row$url)
    expect_equal(project_row$name, exp_row$name)
  })
})


test_that("project_info(zoltar_connection, project_id) returns a list", {
  zoltar_connection <- new_connection("http://example.com")
  project1_json <- two_projects_json[[1]]
  m <- mock(project1_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    the_project_info <- project_info(zoltar_connection, project_id=1L)
    expect_is(the_project_info, "list")
    expect_equal(the_project_info, project1_json)

    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/1")
  })
})


test_that("scores(zoltar_connection, project_id) returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")

  load("scores_response.rda")  # 'scores_response' contains partial response from Impetus_Province_Forecasts-scores.csv
  m <- mock(scores_response)
  testthat::with_mock("httr::GET" = m, {
    the_scores <- scores(zoltar_connection, project_id=1L)
    expect_equal(dim(the_scores), c(380, 10))
    expect_equal(names(the_scores),
      c("model", "timezero", "season", "location", "target", "error", "abs_error", "log_single_bin", "log_multi_bin", "pit"))

    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[1]], "http://example.com/api/project/1/score_data/")
  })
})


test_that("models(zoltar_connection, project_id) returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")

  project1_json <- two_projects_json[[1]]
  model1_json <- jsonlite::read_json("model-1.json")
  m <- mock(project1_json, model1_json, model1_json)  # return values in calling order
  testthat::with_mock("zoltr::get_resource" = m, {
    the_models <- models(zoltar_connection, project_id=1L)

    expect_is(the_models, "data.frame")
    expect_equal(names(the_models), c("id", "url", "project_id", "owner_id", "name", "description", "home_url", "aux_data_url"))
    expect_equal(nrow(the_models), 2)  # 2 projects
    expect_equal(ncol(the_models), 8)

    expect_equal(length(mock_calls(m)), 3)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/1")
    expect_equal(mock_args(m)[[2]][[2]], "http://example.com/api/model/1/")
    expect_equal(mock_args(m)[[3]][[2]], "http://example.com/api/model/2/")
  })
})


#
# ---- model tests ----
#

test_that("model_info(zoltar_connection, model_id) returns a list", {
  zoltar_connection <- new_connection("http://example.com")

  model1_json <- jsonlite::read_json("model-1.json")
  m <- mock(model1_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    exp_model_info_json <- model1_json
    the_model_info <- model_info(zoltar_connection, model_id=1L)
    expect_is(the_model_info, "list")
    expect_equal(the_model_info, exp_model_info_json)

    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/model/1")
  })
})


test_that("forecasts(model_id) returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")

  # note that model-1.json has three 'forecasts' entries, but only one of those has a non-null "forecast" (a url)
  model1_json <- jsonlite::read_json("model-1.json")
  m <- mock(model1_json)
  the_forecasts <- testthat::with_mock("zoltr::get_resource" = m, {
    forecasts(zoltar_connection, model_id=1L)
  })

  expect_is(the_forecasts, "data.frame")
  expect_equal(names(the_forecasts), c("id", "url", "timezero_date", "data_version_date"))
  expect_equal(nrow(the_forecasts), 2)  # 2 forecasts
  expect_equal(ncol(the_forecasts), 4)  # id, url, timezero_date, data_version_date

  expect_equal(length(mock_calls(m)), 1)
  expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/model/1")

  forecast_row <- the_forecasts[1, ]
  exp_row <- data.frame(
    url="http://example.com/api/forecast/71/",
    timezero_date=as.Date("20170117", format="%Y%m%d"),
    data_version_date=as.Date(NA),
    stringsAsFactors=FALSE)
  expect_equal(forecast_row$url, exp_row$url)
  expect_equal(forecast_row$timezero_date, exp_row$timezero_date)
  expect_equal(forecast_row$data_version_date, exp_row$data_version_date)

  forecast_row <- the_forecasts[2, ]
  exp_row <- data.frame(
    url="http://example.com/api/forecast/72/",
    timezero_date=as.Date("20170130", format="%Y%m%d"),
    data_version_date=as.Date("20170131", format="%Y%m%d"),
    stringsAsFactors=FALSE)
  expect_equal(forecast_row$url, exp_row$url)
  expect_equal(forecast_row$timezero_date, exp_row$timezero_date)
  expect_equal(forecast_row$data_version_date, exp_row$data_version_date)
})


test_that("upload_forecast(model_id) returns an UploadFileJob id, and upload_info() is OK", {
  zoltar_connection <- new_connection("http://example.com")

  # test upload_forecast()
  upload_file_job_json <- jsonlite::read_json("upload-file-job-2.json")
  mockery::stub(upload_forecast, 'httr::upload_file', NULL)
  webmockr::stub_request('post', uri='http://example.com/api/model/1/forecasts/') %>%
    to_return(
      body=upload_file_job_json,
      status=200,
      headers=list('Content-Type'='application/json; charset=utf-8'))

  upload_file_job_id <- upload_forecast(zoltar_connection, 1L, NULL, NULL)  # model_id, timezero_date, forecast_csv_file
  expect_equal(upload_file_job_id, 2L)

  exp_upload_file_job_json <- upload_file_job_json
  exp_upload_file_job_json$status <- "SUCCESS"
  exp_upload_file_job_json$created_at <- as.Date("2019-03-26T14:55:31.028436-04:00")
  exp_upload_file_job_json$updated_at <- as.Date("2019-03-26T14:55:37.812924-04:00")
  exp_upload_file_job_json$input_json <- list("forecast_model_pk"=1, "timezero_pk"=2)
  exp_upload_file_job_json$output_json <- list("forecast_pk"=3)

  # test upload_info()
  m <- mock(upload_file_job_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    the_upload_info <- upload_info(zoltar_connection, upload_file_job_id)
    expect_is(the_upload_info, "list")
    expect_equal(the_upload_info, exp_upload_file_job_json)

    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/uploadfilejob/2")
  })
})


test_that("delete_forecast(zoltar_connection, forecast_id) passes correct URL", {
  zoltar_connection <- new_connection("http://example.com")

  load("delete_response.rda")  # 'delete_response' contains 204 response from sample 'DELETE' call
  m <- mock(delete_response)
  testthat::with_mock("httr::DELETE" = m, {
    delete_forecast(zoltar_connection, forecast_id=1L)

    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[1]], "http://example.com/api/forecast/1")
  })
})


#
# ---- forecast tests ----
#

test_that("forecast_info(zoltar_connection, forecast_id) returns a list", {
  zoltar_connection <- new_connection("http://example.com")

  forecast1_json <- jsonlite::read_json("forecast-71.json")
  m <- mock(forecast1_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    the_forecast_info <- forecast_info(zoltar_connection, forecast_id=1L)
    expect_is(the_forecast_info, "list")
    expect_equal(the_forecast_info, forecast1_json)

    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/forecast/1")
  })
})


test_that("forecast_data(zoltar_connection, forecast_id) returns JSON data as a list", {
  zoltar_connection <- new_connection("http://example.com")

  forecast1_json <- jsonlite::read_json("forecast-71.json")
  m <- mock(forecast1_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    the_forecast_data <- forecast_data(zoltar_connection, forecast_id=71L, is_json=TRUE)
    expect_is(the_forecast_data, "list")
    expect_equal(the_forecast_data, forecast1_json)

    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/forecast/71/data/")
  })
})


test_that("forecast_data(zoltar_connection, forecast_id) returns CSV data as a data.frame", {
  zoltar_connection <- new_connection("http://example.com")

  load("data_response.rda")  # 'data_response' contains response from EW1-KoTsarima-2017-01-17-small.csv
  m <- mock(data_response)
  testthat::with_mock("httr::GET" = m, {
    the_data <- forecast_data(zoltar_connection, forecast_id=71L, is_json=FALSE)
    expect_equal(dim(the_data), c(154, 7))
    expect_equal(names(the_data), c("location", "target", "type", "unit", "bin_start_incl", "bin_end_notincl", "value"))

    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[1]], "http://example.com/api/forecast/71/data/")
  })
})


#
# ---- lower-level net-oriented tests ----
#

test_that("new_session() calls get_token() correctly", {
  zoltar_connection <- new_connection("http://example.com")

  called_args <- NULL
  testthat::with_mock("httr::POST" = function(...) {
      called_args <<- list(...)
      load("get_token_response.rda")  # 'get_token_response' contains a 200 response from sample zoltar_authenticate() call
      get_token_response
    },
    zoltar_authenticate(zoltar_connection, "username", "password"))

  expect_equal(called_args$url, "http://example.com/api-token-auth/")
  expect_equal(called_args$body$username, zoltar_connection$username)
  expect_equal(called_args$body$password, zoltar_connection$password)
})


test_that("upload_forecast() passes correct url to POST()", {
  zoltar_connection <- new_connection("http://example.com")

  called_args <- NULL
  testthat::with_mock("zoltr::upload_file" = function(...) {
      NULL
    },
    testthat::with_mock(  # a nested mock because I couldn't find a better way to mock multiple functions at once
      "httr::POST" = function(...) {
        called_args <<- list(...)
        load("upload_response.rda")  # 'upload_response' contains 200 response from sample upload_forecast() call
        upload_response
      },
    upload_forecast(zoltar_connection, 1L, NULL, NULL)))  # model_id, timezero_date, forecast_csv_file

  expect_equal(called_args$url, "http://example.com/api/model/1/forecasts/")
})

