context("connection")
library(jsonlite)


#
# ---- todos ----
#
# REF: expect_equal(actual, expected)
#

# todo need to mock anything that hits the net:
# - data(forecast, is_json=TRUE)
# - delete(zoltar_resource)
# - get_token(zoltar_session)
# - json_for_uri(zoltar_connection)
# - upload_forecast(model, timezero_date, forecast_csv_file)


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


#
# ---- tests ----
#

test_that("json_for_uri(zoltar_connection) builds correct requests", {
  # projects(zoltar_connection)
  # refresh(zoltar_resource)
  # data(forecast)
  # ...
  skip("todo")
})


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
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)

  project1_json <- two_projects_json[[1]]
  project1 <- with_mock(
    "json_for_uri" = function(...) {
      project1_json
    },
    new_project(zoltar_connection, project1_json$url))

  the_models <- with_mock(
    "refresh" = function(...) {  # we don't care about the model's json, just its uri
    },
    models(project1))

  expect_equal(length(the_models), 2)
  for (idx in 1:2) {  # NB: assumes order is preserved from json
    the_model <- the_models[[idx]]
    model_uri <- project1_json$models[[idx]]
    expect_is(the_model, "Model")
    expect_equal(the_model$uri, model_uri)
  }
})


test_that("name(project) returns the name", {
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)

  project1_json <- two_projects_json[[1]]
  project1 <- with_mock(
    "json_for_uri" = function(...) {
      project1_json
    },
    new_project(zoltar_connection, project1_json$url))

  expect_equal(name(project1), project1_json$name)
})


test_that("forecasts(model) returns a list of Forecast objects", {
  # note that model-1.json has three 'forecasts' entries, but only one of those has a non-null "forecast" (a URI)
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)

  model1_json <- jsonlite::read_json("model-1.json")
  model1 <- with_mock(
    "json_for_uri" = function(...) {  # also incorrectly used to refresh() each Model's json, but we don't care here
      model1_json
    },
    new_model(zoltar_connection, model1_json$url))

  the_forecasts <- with_mock(
    "refresh" = function(...) {  # we don't care about the model's json, just its uri
    },
    forecasts(model1))

  expect_equal(length(the_forecasts), 1)

  the_forecast <- the_forecasts[[1]]
  expect_is(the_forecast, "Forecast")
  expect_equal(the_forecast$uri, model1_json$forecasts[[2]]$forecast)
})


test_that("forecast_for_pk(model) returns a Forecast object", {
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)

  model1_json <- jsonlite::read_json("model-1.json")
  model1 <- with_mock(
    "json_for_uri" = function(...) {  # also incorrectly used to refresh() each Model's json, but we don't care here
      model1_json
    },
    new_model(zoltar_connection, model1_json$url))


  the_forecast <- with_mock(
    "refresh" = function(...) {  # we don't care about the model's json, just its uri
    },
    forecast_for_pk(model1, 71))  # forecast_pk from model1_json$forecasts[[2]]$forecast

  browser()
  expect_is(the_forecast, "Forecast")
  expect_equal(the_forecast$uri, model1_json$forecasts[[2]]$forecast)
})


test_that("upload_forecast(model) returns an UploadFileJob object", {
  skip("todo")
})


test_that("data(forecast) returns JSON or CSV data", {
  # is_json=TRUE
  # is_json=FALSE
  skip("todo")
})


test_that("timezero_date(forecast) returns the timezero_date", {
  skip("todo")
})


test_that("csv_filename(forecast) returns the csv_filename", {
  skip("todo")
})


test_that("status_as_str(upload_file_job) returns the status_as_str", {
  skip("todo")
})


test_that("output_json(upload_file_job) returns the output_json", {
  skip("todo")
})

