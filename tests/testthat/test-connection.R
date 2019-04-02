context("connection")
library(jsonlite)


# ---- todos ----

# REF: expect_equal(actual, expected)

# todo need to mock anything that hits the net:
# - data(forecast, is_json=TRUE)
# - delete(zoltar_resource)
# - get_token(zoltar_session)
# - json_for_uri(zoltar_connection)
# - upload_forecast(model, timezero_date, forecast_csv_file)


# ---- utilities ----

mock_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"

mock_authenticate <- function(zoltar_connection) {
  with_mock(
  "get_token" = function(...) {
    mock_token
  },
  z_authenticate(zoltar_connection, "username", "password")
  )
}


# ---- tests ----

test_that("json_for_uri(zoltar_connection) builds correct requests", {
  # projects(zoltar_connection)
  # refresh(zoltar_resource)
  # data(forecast)
  # ...
  fail("todo")
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


# todo re: loading the expected json: save as R data file? https://www.mango-solutions.com/blog/testing-without-the-internet-using-mock-functions
test_that("projects(zoltar_connection) returns a list of Project objects", {
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)

  two_projects_json <- jsonlite::read_json("two-projects.json")
  the_projects <- with_mock(
    "json_for_uri" = function(...) {  # also incorrectly used to refresh() each Project's json, but we don't care here
        two_projects_json
    },
    projects(zoltar_connection)
  )

  expect_equal(length(the_projects), 2)
  for (idx in 1:2) {  # NB: assumes order is preserved from json
    project_json <- two_projects_json[[idx]]
    project <- the_projects[[idx]]
    expect_is(project, "Project")
    expect_equal(project$url, project_json$uri)
  }
})


test_that("models(project) returns a list of Model objects", {
  fail("todo")
})


test_that("name(project) returns the name", {
  fail("todo")
})


test_that("forecasts(model) returns a list of Forecast objects", {
  fail("todo")
})


test_that("forecast_for_pk(model) returns a Forecast object", {
  fail("todo")
})


test_that("upload_forecast(model) returns an UploadFileJob object", {
  fail("todo")
})


test_that("data(forecast) returns JSON or CSV data", {
  # is_json=TRUE
  # is_json=FALSE
  fail("todo")
})


test_that("timezero_date(forecast) returns the timezero_date", {
  fail("todo")
})


test_that("csv_filename(forecast) returns the csv_filename", {
  fail("todo")
})


test_that("status_as_str(upload_file_job) returns the status_as_str", {
  fail("todo")
})


test_that("output_json(upload_file_job) returns the output_json", {
  fail("todo")
})

