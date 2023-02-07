context("project")

library(httr)
library(jsonlite)
library(testthat)
library(webmockr)
library(zoltr)


TWO_PROJECTS_JSON <- jsonlite::read_json("data/projects-list.json")

test_that("create_project() creates a Project", {
  zoltar_connection <- new_connection("http://example.com")
  webmockr::stub_request("post", uri = "http://example.com/api/projects/") %>%
    to_return(
      body = TWO_PROJECTS_JSON[[1]],
      status = 200,
      headers = list('Content-Type' = 'application/json; charset=utf-8'))
  project_config <- jsonlite::read_json("data/cdc-project.json")
  testthat::with_mock("httr::POST" = function(...) {
    called_args <<- list(...)
    load("data/get_token_response.rda")  # 'response' contains a 200 response from a sample `get_token()` call via `zoltar_authenticate()`
    response  # actual response doesn't matter, just its class
  },
                      create_project(zoltar_connection, project_config))
  expect_equal(called_args$url, "http://example.com/api/projects/")
  expect_equal(called_args$body, jsonlite::toJSON(list(project_config = project_config),
                                                  auto_unbox = TRUE, null = "null"))
})


test_that("create_project() calls re_authenticate_if_necessary(), and returns a Project URL", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    webmockr::stub_request("post", uri = "http://example.com/api/projects/") %>%
      to_return(
        body = TWO_PROJECTS_JSON[[1]],
        status = 200,
        headers = list('Content-Type' = 'application/json; charset=utf-8'))
    project_config <- jsonlite::read_json("data/cdc-project.json")
    project_url <- create_project(zoltar_connection, project_config)
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(project_url, "http://example.com/api/project/1/")
  })
})


test_that("submit_query() calls re_authenticate_if_necessary()", {
  zoltar_connection <- new_connection("http://example.com")

  # test `query_type = "forecasts"` POSTs to correct uri
  m <- mock()
  # NB: todo this overrides `test_that("submit_query() creates a Job"` !?:
  job_json <- jsonlite::read_json("data/job-2.json")
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    webmockr::stub_request("post", uri = "http://example.com/api/project/1/forecast_queries/") %>%
      to_return(
        body = job_json,
        status = 200,
        headers = list('Content-Type' = 'application/json; charset=utf-8'))
    submit_query(zoltar_connection, "http://example.com/api/project/1/", "forecasts", list())
    expect_equal(length(mock_calls(m)), 1)
  })


  # test `query_type = "truth"` POSTs to correct uri
  m <- mock()
  # NB: todo this overrides `test_that("submit_query() creates a Job"` !?:
  job_json <- jsonlite::read_json("data/job-2.json")
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    webmockr::stub_request("post", uri = "http://example.com/api/project/1/truth_queries/") %>%
      to_return(
        body = job_json,
        status = 200,
        headers = list('Content-Type' = 'application/json; charset=utf-8'))
    submit_query(zoltar_connection, "http://example.com/api/project/1/", "truth", list())
    expect_equal(length(mock_calls(m)), 1)
  })
})


test_that("delete_project() calls delete_resource", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  testthat::with_mock("zoltr::delete_resource" = m, {
    delete_project(zoltar_connection, "http://example.com/api/project/0/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/0/")
  })
})


test_that("models() returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")
  models_list_json <- jsonlite::read_json("data/models-list.json")
  m <- mock(models_list_json)  # return values in calling order
  testthat::with_mock("zoltr::get_resource" = m, {
    the_models <- models(zoltar_connection, "http://example.com/api/project/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/1/models/")
    expect_is(the_models, "data.frame")
    expect_equal(names(the_models), c("id", "url", "project_url", "owner_url", "name", "model_abbr", "notes",
                                      "description", "home_url", "aux_data_url"))
    expect_equal(nrow(the_models), 1)  # 1 model
    expect_equal(ncol(the_models), 10)
  })
})


test_that("zoltar_units() returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")
  units_list_json <- jsonlite::read_json("data/units-list.json")
  m <- mock(units_list_json)  # return values in calling order
  testthat::with_mock("zoltr::get_resource" = m, {
    the_units <- zoltar_units(zoltar_connection, "http://example.com/api/project/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/1/units/")
    expect_is(the_units, "data.frame")
    expect_equal(names(the_units), c("id", "url", "abbreviation", "name"))
    expect_equal(nrow(the_units), 3)  # 3 units
    expect_equal(ncol(the_units), 4)
  })
})


test_that("targets() returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")
  targets_list_json <- jsonlite::read_json("data/targets-list.json")
  m <- mock(targets_list_json)  # return values in calling order
  testthat::with_mock("zoltr::get_resource" = m, {
    the_targets <- targets(zoltar_connection, "http://example.com/api/project/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/1/targets/")
    expect_is(the_targets, "data.frame")
    expect_equal(names(the_targets), c("id", "url", "name", "type", "description", "outcome_variable",
                                       "is_step_ahead", "numeric_horizon", "reference_date_type"))
    expect_equal(nrow(the_targets), 5)  # 5 targets
    expect_equal(ncol(the_targets), 9)
  })
})


test_that("timezeros() returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")
  timezeros_list_json <- jsonlite::read_json("data/timezeros-list.json")
  m <- mock(timezeros_list_json)  # return values in calling order
  testthat::with_mock("zoltr::get_resource" = m, {
    the_timezeros <- timezeros(zoltar_connection, "http://example.com/api/project/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/1/timezeros/")
    expect_is(the_timezeros, "data.frame")
    expect_equal(names(the_timezeros), c("id", "url", "timezero_date", "data_version_date", "is_season_start",
                                         "season_name"))
    expect_equal(nrow(the_timezeros), 3)  # 5 timezeros
    expect_equal(ncol(the_timezeros), 6)

    timezero_row <- the_timezeros[1,]
    exp_row <- data.frame(id = 5L, url = "http://example.com/api/timezero/5/",
                          timezero_date = as.Date("2011-10-02", YYYY_MM_DD_DATE_FORMAT),
                          data_version_date = as.Date("2011-10-03", YYYY_MM_DD_DATE_FORMAT),
                          is_season_start = TRUE, season_name = "2011-2012", stringsAsFactors = FALSE)
    rownames(timezero_row) <- c()
    rownames(exp_row) <- c()
    expect_equal(timezero_row, exp_row)

    timezero_row <- the_timezeros[2,]
    exp_row <- data.frame(id = 6L, url = "http://example.com/api/timezero/6/",
                          timezero_date = as.Date("2011-10-09", YYYY_MM_DD_DATE_FORMAT),
                          data_version_date = as.Date(NA), is_season_start = FALSE, season_name = as.character(NA),
                          stringsAsFactors = FALSE)
    rownames(timezero_row) <- c()
    rownames(exp_row) <- c()
    expect_equal(timezero_row, exp_row)
  })
})


#
# ---- test query functions ----
#

test_that("submit_query() creates a Job", {
  zoltar_connection <- new_connection("http://example.com")

  # test correct url and args passed
  job_json <- jsonlite::read_json("data/job-2.json")
  webmockr::stub_request("post", uri = "http://example.com/api/project/1/forecast_queries/") %>%
    to_return(
      body = job_json,
      status = 200,
      headers = list('Content-Type' = 'application/json; charset=utf-8'))

  query <- list()
  testthat::with_mock("httr::POST" = function(...) {
    called_args <<- list(...)
    load("data/upload_response.rda")  # 'response' contains 200 response from sample upload_truth() call
    response  # actual response doesn't matter, just its class
  },
                      submit_query(zoltar_connection, "http://example.com/api/project/1/", "forecasts", query))
  expect_equal(called_args$url, "http://example.com/api/project/1/forecast_queries/")
  expect_equal(as.character(called_args$body), "{\"query\":{}}")  # due to httr/jsonlite fighting

  # test a job url is returned
  job_url <- submit_query(zoltar_connection, "http://example.com/api/project/1/", "forecasts", query)
  expect_equal(job_url, "http://example.com/api/job/2/")
})


test_that("json_for_query() is correct", {
  query_exp_chrs <- list(list(query = list(),
                              exp_chr = "{\"query\":{}}"),
                         list(query = list("models" = list()),
                              exp_chr = "{\"query\":{\"models\":[]}}"),
                         list(query = list("models" = list(1), "units" = list(2, 3)),
                              exp_chr = "{\"query\":{\"models\":[1],\"units\":[2,3]}}"),
                         list(query = list("types" = list("point", "quantile")),
                              exp_chr = "{\"query\":{\"types\":[\"point\",\"quantile\"]}}"),
                         list(query = list("types" = list("point")),
                              exp_chr = "{\"query\":{\"types\":[\"point\"]}}")
  )
  for (row in query_exp_chrs) {
    expect_equal(as.character(json_for_query(row$query)), row$exp_chr)
  }
})


#
# ---- test info functions ----
#

test_that("project_info() returns a list", {
  zoltar_connection <- new_connection("http://example.com")
  exp_project_info <- TWO_PROJECTS_JSON[[1]]
  m <- mock(exp_project_info)
  testthat::with_mock("zoltr::get_resource" = m, {
    act_truth_info <- project_info(zoltar_connection, "http://example.com/api/project/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/1/")
    expect_is(act_truth_info, "list")
    expect_equal(act_truth_info, exp_project_info)
  })
})


test_that("target_info() returns a list", {
  zoltar_connection <- new_connection("http://example.com")
  targets_list_json <- jsonlite::read_json("data/targets-list.json")
  exp_target_info <- targets_list_json[[1]]
  m <- mock(exp_target_info)
  testthat::with_mock("zoltr::get_resource" = m, {
    act_target_info <- target_info(zoltar_connection, "http://example.com/api/target/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/target/1/")
    expect_is(act_target_info, "list")
    expect_equal(act_target_info, exp_target_info)
  })
})


test_that("timezero_info() returns a list", {
  zoltar_connection <- new_connection("http://example.com")
  timezeros_list_json <- jsonlite::read_json("data/timezeros-list.json")
  exp_timezero_info <- timezeros_list_json[[1]]
  m <- mock(exp_timezero_info)
  testthat::with_mock("zoltr::get_resource" = m, {
    act_timezero_info <- timezero_info(zoltar_connection, "http://example.com/api/timezero/2/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/timezero/2/")
    expect_is(act_timezero_info, "list")
    expect_equal(act_timezero_info, exp_timezero_info)
  })
})


test_that("unit_info() returns a list", {
  zoltar_connection <- new_connection("http://example.com")
  units_list_json <- jsonlite::read_json("data/units-list.json")
  exp_unit_info <- units_list_json[[1]]
  m <- mock(exp_unit_info)
  testthat::with_mock("zoltr::get_resource" = m, {
    act_unit_info <- unit_info(zoltar_connection, "http://example.com/api/unit/3/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/unit/3/")
    expect_is(act_unit_info, "list")
    expect_equal(act_unit_info, exp_unit_info)
  })
})


test_that("truth_info() returns a list", {
  zoltar_connection <- new_connection("http://example.com")

  truth_info_json <- jsonlite::read_json("data/truth-info.json")
  m <- mock(truth_info_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    exp_truth_info <- truth_info_json
    exp_truth_info$created_at <- as.Date(exp_truth_info$created_at)
    act_truth_info <- truth_info(zoltar_connection, "http://example.com/api/project/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/1/truth/")
    expect_is(act_truth_info, "list")
    expect_equal(act_truth_info, exp_truth_info)
  })
})


#
# ---- test upload functions ----
#

test_that("upload_truth() creates a Job", {
  zoltar_connection <- new_connection("http://example.com")
  job_json <- jsonlite::read_json("data/job-2.json")
  mockery::stub(upload_truth, 'httr::upload_file', NULL)
  webmockr::stub_request("post", uri = "http://example.com/api/project/1/truth/") %>%
    to_return(
      body = job_json,
      status = 200,
      headers = list('Content-Type' = 'application/json; charset=utf-8'))

  testthat::with_mock("httr::POST" = function(...) {
    called_args <<- list(...)
    load("data/get_token_response.rda")  # 'response' contains a 200 response from a sample `get_token()` call via `zoltar_authenticate()`
    response  # actual response doesn't matter, just its class
  },
                      upload_truth(zoltar_connection, "http://example.com/api/project/1/",
                                   "data/docs-ground-truth-non-dup.csv"))
  expect_equal(called_args$url, "http://example.com/api/project/1/truth/")
  expect_equal(called_args$body$data_file, NULL)  # due to mockery::stub() calls elsewhere
})


test_that("upload_truth() returns a Job URL, and job_info() is correct", {
  # NB: following uses project/2 so that `stub_request` returns below job_json and not above project/1 one
  zoltar_connection <- new_connection("http://example.com")
  job_json <- jsonlite::read_json("data/job-1.json")
  mockery::stub(upload_truth, 'httr::upload_file', NULL)
  webmockr::stub_request("post", uri = "http://example.com/api/project/2/truth/") %>%
    to_return(
      body = job_json,
      status = 200,
      headers = list('Content-Type' = 'application/json; charset=utf-8'))
  job_url <- upload_truth(zoltar_connection, "http://example.com/api/project/2/",
                          "data/docs-ground-truth-non-dup.csv")
  expect_equal(job_url, "http://example.com/api/job/1/")

  # test job_info()
  exp_job_json <- job_json
  exp_job_json$status <- "SUCCESS"
  exp_job_json$created_at <- lubridate::parse_date_time(
    "2022-10-24T14:02:01.679789-04:00", DATE_TIME_TZ_FORMAT, exact = TRUE)
  exp_job_json$updated_at <- lubridate::parse_date_time(
    "2022-10-24T14:02:02.464451-04:00", DATE_TIME_TZ_FORMAT, exact = TRUE)
  exp_job_json$input_json <- list("type" = "UPLOAD_TRUTH", filename = "docs-ground-truth-non-dup.csv",
                                  "project_pk" = "41")
  exp_job_json$output_json <- list("num_rows" = 11,
                                   "missing_units" = list("loc1x", "loc2x"),
                                   "num_forecasts" = 3,
                                   "missing_targets" = list("cases next weekx"),
                                   "missing_time_zeros" = list(as.Date("2022-10-01", YYYY_MM_DD_DATE_FORMAT),
                                                               as.Date("2022-10-02", YYYY_MM_DD_DATE_FORMAT)))

  m <- mock(job_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    the_job_info <- job_info(zoltar_connection, "http://example.com/api/job/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/job/1/")
    expect_is(the_job_info, "list")
    expect_equal(the_job_info, exp_job_json)
  })
})


test_that("upload_truth() calls re_authenticate_if_necessary()", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    upload_truth(zoltar_connection, "http://example.com/api/project/1/",
                 "data/docs-ground-truth-non-dup.csv")
    expect_equal(length(mock_calls(m)), 1)
  })
})


test_that("upload_truth() passes correct url to POST()", {
  zoltar_connection <- new_connection("http://example.com")
  called_args <- NULL
  # Note: this file is a duplicate of vignettes one b/c I could not figure out how to access that directory for both
  # devtools::test() and devtools::check() (different working dirs):
  testthat::with_mock(
    "httr::POST" = function(...) {
      called_args <<- list(...)
      load("data/upload_response.rda")  # 'response' contains 200 response from sample upload_truth() call
      response
    },
    job_url <- upload_truth(zoltar_connection, "http://example.com/api/project/1/",
                            "data/docs-ground-truth-non-dup.csv"))
  expect_equal(called_args$url, "http://example.com/api/project/1/truth/")
  expect_s3_class(called_args$body$data_file, "form_file")
})


test_that("upload_truth() passes correct body to POST() for issued_at combinations", {
  zoltar_connection <- new_connection("http://example.com")
  called_args <- NULL
  # Note: this file is a duplicate of vignettes one b/c I could not figure out how to access that directory for both
  # devtools::test() and devtools::check() (different working dirs):

  # case: issued_at = NULL
  testthat::with_mock(
    "httr::POST" = function(...) {
      called_args <<- list(...)
      load("data/upload_response.rda")  # 'response' contains 200 response from sample upload_truth() call
      response
    },
    job_url <- upload_truth(zoltar_connection, "http://example.com/api/project/1/",
                            "data/docs-ground-truth-non-dup.csv"))
  expect_false("issued_at" %in% names(called_args))

  # case: issued_at = datetime
  issued_at <- "the issued_at"
  testthat::with_mock(
    "httr::POST" = function(...) {
      called_args <<- list(...)
      load("data/upload_response.rda")  # 'response' contains 200 response from sample upload_truth() call
      response
    },
    job_url <- upload_truth(zoltar_connection, "http://example.com/api/project/1/",
                            "data/docs-ground-truth-non-dup.csv", issued_at = issued_at))
  expect_equal(called_args$body$issued_at, issued_at)
})


test_that("upload_truth() requires a existing file", {
  # just a simple test to drive converting upload_truth() from file to list
  zoltar_connection <- new_connection("http://example.com")
  expect_error(upload_truth(zoltar_connection, "http://example.com/api/project/1/",
                            "data/not-a-real-file.csv"),
               "file.exists(path) is not TRUE", fixed = TRUE)
})


#
# ---- test create_timezero() ----
#

test_that("create_timezero() creates a TimeZero", {
  zoltar_connection <- new_connection("http://example.com")
  timezeros_list_json <- jsonlite::read_json("data/timezeros-list.json")
  timezero_info <- timezeros_list_json[[1]]    # "is_season_start": true, "season_name": "2011-2012"

  # test is_season_start=TRUE
  timezero_config <- list(timezero_date = "2022-11-08", data_version_date = "2022-11-09", is_season_start = TRUE,
                          season_name = "Fall 2022")
  webmockr::stub_request("post", uri = "http://example.com/api/project/1/timezeros/") %>%
    to_return(
      body = timezero_info,
      status = 200,
      headers = list('Content-Type' = 'application/json; charset=utf-8'))
  testthat::with_mock("httr::POST" = function(...) {
    called_args <<- list(...)
    load("data/get_token_response.rda")  # 'response' contains a 200 response from a sample `get_token()` call via `zoltar_authenticate()`
    response  # actual response doesn't matter, just its class
  },
                      create_timezero(zoltar_connection, "http://example.com/api/project/1/",
                                      timezero_config$timezero_date, timezero_config$data_version_date,
                                      timezero_config$is_season_start, timezero_config$season_name))
  expect_equal(called_args$url, "http://example.com/api/project/1/timezeros/")
  expect_equal(called_args$body$timezero_config, timezero_config)

  # test is_season_start=FALSE
  timezero_info <- timezeros_list_json[[2]]  # "is_season_start": false
  timezero_config <- list(timezero_date = "2022-11-08", data_version_date = "2022-11-09", is_season_start = FALSE)
  webmockr::stub_request("post", uri = "http://example.com/api/project/1/timezeros/") %>%
    to_return(
      body = timezero_info,
      status = 200,
      headers = list('Content-Type' = 'application/json; charset=utf-8'))
  testthat::with_mock("httr::POST" = function(...) {
    called_args <<- list(...)
    load("data/get_token_response.rda")  # 'response' contains a 200 response from a sample `get_token()` call via `zoltar_authenticate()`
    response  # actual response doesn't matter, just its class
  },
                      create_timezero(zoltar_connection, "http://example.com/api/project/1/",
                                      timezero_config$timezero_date, timezero_config$data_version_date,
                                      timezero_config$is_season_start))  # no season_name
  expect_equal(called_args$url, "http://example.com/api/project/1/timezeros/")
  expect_equal(called_args$body$timezero_config, timezero_config)
})


test_that("create_timezero() calls re_authenticate_if_necessary() and returns a TimeZero URL", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  timezeros_list_json <- jsonlite::read_json("data/timezeros-list.json")
  timezero_info <- timezeros_list_json[[1]]    # "is_season_start": true, "season_name": "2011-2012"
  timezero_config <- list(timezero_date = "2022-11-08", data_version_date = "2022-11-09", is_season_start = TRUE,
                          season_name = "Fall 2022")
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    webmockr::stub_request("post", uri = "http://example.com/api/project/1/timezeros/") %>%
      to_return(
        body = timezero_info,
        status = 200,
        headers = list('Content-Type' = 'application/json; charset=utf-8'))
    timezero_url <- create_timezero(zoltar_connection, "http://example.com/api/project/1/",
                                    timezero_config$timezero_date, timezero_config$data_version_date,
                                    timezero_config$is_season_start, timezero_config$season_name)
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(timezero_url, "http://example.com/api/timezero/5/")
  })
})
