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
  expect_equal(called_args$body$project_config, project_config)
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

  # test `is_forecast_query = "forecasts"` POSTs to correct uri
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


  # test `is_forecast_query = "scores"` POSTs to correct uri
  m <- mock()
  # NB: todo this overrides `test_that("submit_query() creates a Job"` !?:
  job_json <- jsonlite::read_json("data/job-2.json")
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    webmockr::stub_request("post", uri = "http://example.com/api/project/1/scores_queries/") %>%
      to_return(
        body = job_json,
        status = 200,
        headers = list('Content-Type' = 'application/json; charset=utf-8'))
    submit_query(zoltar_connection, "http://example.com/api/project/1/", "scores", list())
    expect_equal(length(mock_calls(m)), 1)
  })


  # test `is_forecast_query = "truth"` POSTs to correct uri
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
    expect_equal(names(the_units), c("id", "url", "name"))
    expect_equal(nrow(the_units), 3)  # 3 units
    expect_equal(ncol(the_units), 3)
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
    expect_equal(names(the_targets), c("id", "url", "name", "description", "type", "is_step_ahead",
                                       "step_ahead_increment", "unit"))
    expect_equal(nrow(the_targets), 5)  # 5 targets
    expect_equal(ncol(the_targets), 8)
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
    load("data/upload_response.rda")  # 'response' contains 200 response from sample upload_forecast() call
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
