context("connection")
library(jsonlite)


#
# ---- utilities ----
#
# NB: these assume that this file is loaded in order, i.e., that they are called before any tests
#

two_projects_json <- jsonlite::read_json("projects-list.json")

mock_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"


mock_authenticate <- function(zoltar_connection) {
  with_mock(
  "get_token" = function(...) {
    mock_token
  },
  z_authenticate(zoltar_connection, "username", "password"))
}


mock_project <- function() {
  # helper that returns a list of two items: project: a mock Project, and json: the test json used to create the project
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)
  project1_json <- two_projects_json[[1]]
  project1 <- with_mock(
  "json_for_uri" = function(...) {
      project1_json
    },
    new_project(zoltar_connection, project1_json$url))
  list(project=project1, json=project1_json)
}


mock_model <- function() {
  # helper that returns a list of two items: model: a mock Model, and json: the test json used to create the model
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)
  model1_json <- jsonlite::read_json("model-1.json")
  model1 <- with_mock(
    "json_for_uri" = function(...) {
      model1_json
    },
    new_model(zoltar_connection, model1_json$url))
  list(model=model1, json=model1_json)
}


mock_forecast <- function() {
  # helper that returns a list of two items: forecast: a mock Forecast, and json: the test json used to create the forecast
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)
  forecast_json <- jsonlite::read_json("forecast-71.json")
  the_forecast <- with_mock(
    "json_for_uri" = function(...) {
      forecast_json
    },
  new_forecast(zoltar_connection, forecast_json$url))
  list(forecast=the_forecast, json=forecast_json)
}


#
# ---- lower-level net-oriented tests ----
#

test_that("functions calling constructors should pass correct uris to new_resource()", {
  # projects()
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)
  called_uris <- list()
  the_projects <- with_mock(
    "json_for_uri" = function(zoltar_connection, uri, ...) {
      called_uris <<- append(called_uris, uri)
      two_projects_json
    },
    projects(zoltar_connection))

  expected_called_uris <- list(
    "http://example.com/api/projects/",
    "http://example.com/api/project/1/",
    "http://example.com/api/project/2/")
  expect_equal(called_uris, expected_called_uris)


  # models()
  project_and_json <- mock_project()
  the_project <- project_and_json[['project']]
  called_uris <- list()
  with_mock(
    "json_for_uri" = function(zoltar_connection, uri, ...) {
      called_uris <<- append(called_uris, uri)
    },
    models(the_project))

  expected_called_uris <- list(
    "http://example.com/api/model/1/",
    "http://example.com/api/model/2/")
  expect_equal(called_uris, expected_called_uris)


  # forecasts()
  model_and_json <- mock_model()
  the_model <- model_and_json[['model']]
  called_uris <- list()
  with_mock(
    "json_for_uri" = function(zoltar_connection, uri, ...) {
      called_uris <<- append(called_uris, uri)
    },
    forecasts(the_model))
  expected_called_uris <- list("http://example.com/api/forecast/71/")
  expect_equal(called_uris, expected_called_uris)


  # forecast_for_pk()
  called_uris <- list()
  with_mock(
    "json_for_uri" = function(zoltar_connection, uri, ...) {
      called_uris <<- append(called_uris, uri)
    },
    forecast_for_pk(the_model, 71))  # forecast_pk from model1_json$forecasts[[2]]$forecast
  expected_called_uris <- list("http://example.com/api/forecast/71/")
  expect_equal(called_uris, expected_called_uris)


  # upload_forecast()
  upload_file_job_json <- jsonlite::read_json("upload-file-job-2.json")
  called_uris <- list()
  with_mock(
    "post_forecast" = function(...) {
      upload_file_job_json
    },
    with_mock(  # a nested mock because I couldn't find a better way to mock multiple functions at once
    "json_for_uri" = function(zoltar_connection, uri, ...) {
        called_uris <<- append(called_uris, uri)
      },
      upload_forecast(the_model, NULL, NULL)  # timezero_date, forecast_csv_file
    )
  )
  expected_called_uris <- list("http://example.com/api/uploadfilejob/2/")
  expect_equal(called_uris, expected_called_uris)
})


test_that("new_session() calls get_token() correctly", {
  # new_session()
  #   get_token()
  skip("todo")
})


test_that("post_forecast() passes correct URI to POST()", {
  # post_forecast()
  #   httr::POST()    # paste0(model$uri, 'forecasts/')
  skip("todo")
})


test_that("data() passes correct URI to json_for_uri() and GET()", {
  #   data()
  #     json_for_uri(forecast$zoltar_connection, data_uri)
  #     httr::GET()     # data_uri
  skip("todo")
})


#
# ---- OO-level tests ----
#

test_that("new_connection() returns a ZoltarConnection object", {
  zoltar_connection <- new_connection("http://example.com")
  expect_is(zoltar_connection, "ZoltarConnection")
})


test_that("z_authenticate(zoltar_connection) saves username, password, and session", {
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)
  expect_equal(zoltar_connection$username, "username")
  expect_equal(zoltar_connection$password, "password")
  expect_is(zoltar_connection$session, "ZoltarSession")
  expect_equal(zoltar_connection$session$token, mock_token)
})


test_that("projects(zoltar_connection) returns a list of Project objects", {
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)
  the_projects <- with_mock(
    "json_for_uri" = function(...) {  # also incorrectly used to refresh() each Project's json, but we don't care here
      two_projects_json
    },
    projects(zoltar_connection))
  expect_equal(length(the_projects), 2)

  for (idx in 1:2) {  # NB: assumes order is preserved from json
    the_project <- the_projects[[idx]]
    project_json <- two_projects_json[[idx]]
    expect_is(the_project, "Project")
    expect_equal(the_project$uri, project_json$url)
  }
})


test_that("models(project) returns a list of Model objects", {
  project_and_json <- mock_project()
  the_project <- project_and_json[['project']]
  project_json <- project_and_json[['json']]
  the_models <- with_mock(
    "refresh" = function(...) {  # we don't care about the model's json, just its uri
    },
    models(the_project))
  expect_equal(length(the_models), 2)

  for (idx in 1:2) {  # NB: assumes order is preserved from json
    the_model <- the_models[[idx]]
    model_uri <- project_json$models[[idx]]
    expect_is(the_model, "Model")
    expect_equal(the_model$uri, model_uri)
  }
})


test_that("name(project) returns the name", {
  project_and_json <- mock_project()
  the_project <- project_and_json[['project']]
  project_json <- project_and_json[['json']]
  expect_equal(name(the_project), project_json$name)
})


test_that("forecasts(model) returns a list of Forecast objects", {
  # note that model-1.json has three 'forecasts' entries, but only one of those has a non-null "forecast" (a URI)
  model_and_json <- mock_model()
  the_model <- model_and_json[['model']]
  model_json <- model_and_json[['json']]
  the_forecasts <- with_mock(
    "refresh" = function(...) {  # we don't care about the model's json, just its uri
    },
    forecasts(the_model))
  expect_equal(length(the_forecasts), 1)

  the_forecast <- the_forecasts[[1]]
  expect_is(the_forecast, "Forecast")
  expect_equal(the_forecast$uri, model_json$forecasts[[2]]$forecast)
})


test_that("forecast_for_pk(model) returns a Forecast object", {
  model_and_json <- mock_model()
  the_model <- model_and_json[['model']]
  model_json <- model_and_json[['json']]
  the_forecast <- with_mock(
    "refresh" = function(...) {  # we don't care about the forecast's json, just its uri
    },
    forecast_for_pk(the_model, 71))  # forecast_pk from model1_json$forecasts[[2]]$forecast
  expect_is(the_forecast, "Forecast")
  expect_equal(the_forecast$uri, model_json$forecasts[[2]]$forecast)
})


test_that("upload_forecast(model) returns an UploadFileJob object, with correct status_as_str()", {
  model_and_json <- mock_model()
  the_model <- model_and_json[['model']]
  upload_file_job_json <- jsonlite::read_json("upload-file-job-2.json")
  the_upload_file_job <- with_mock(
    "post_forecast" = function(...) {
      upload_file_job_json
    },
    with_mock(  # a nested mock because I couldn't find a better way to mock multiple functions at once
      "json_for_uri" = function(...) {
        upload_file_job_json
      },
      upload_forecast(the_model, NULL, NULL)  # timezero_date, forecast_csv_file
    )
  )

  expect_is(the_upload_file_job, "UploadFileJob")
  expect_equal(the_upload_file_job$uri, upload_file_job_json$url)
  expect_equal(status_as_str(the_upload_file_job), "SUCCESS")
  expect_equal(output_json(the_upload_file_job), list("forecast_pk" = 3))
})


test_that("data(forecast) returns JSON data", {
  # to test json format we simply test that json_for_uri() is called with the correct URI
  forecast_and_json <- mock_forecast()
  the_forecast <- forecast_and_json[['forecast']]

  called_uri <- NULL
  the_data <- with_mock(
    "json_for_uri" = function(zoltar_connection, uri, ...) {
      called_uri <<- uri
      NULL  # don't care about return value
    },
    data(the_forecast, is_json=TRUE))

  expect_equal(called_uri, "http://example.com/api/forecast/71/data/")
})


test_that("data(forecast) returns CSV data", {
  forecast_and_json <- mock_forecast()
  the_forecast <- forecast_and_json[['forecast']]

  the_data <- with_mock(
    "httr::GET" = function(...) {
      load("data_response.rda")  # 'data_response' contains response from EW1-KoTsarima-2017-01-17-small.csv
      data_response
    },
    data(the_forecast, is_json=FALSE))

  expect_equal(dim(the_data), c(154, 7))
  expect_equal(names(the_data), c("location", "target", "type", "unit", "bin_start_incl", "bin_end_notincl", "value"))
})


test_that("timezero_date(forecast) and csv_filename(forecast) return correct", {
  forecast_and_json <- mock_forecast()
  the_forecast <- forecast_and_json[['forecast']]
  forecast_json <- forecast_and_json[['json']]
  expect_equal(timezero_date(the_forecast), forecast_json$time_zero$timezero_date)
  expect_equal(csv_filename(the_forecast), forecast_json$csv_filename)
})

