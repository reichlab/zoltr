context("connection")

library(base64url)
library(httr)
library(jsonlite)
library(mockery)
library(testthat)
library(webmockr)
library(zoltr)

httr_mock()  # turns on for *all* tests


#
# ---- utilities ----
#
# NB: these assume that this file is loaded in order, i.e., that they are called before any tests
#

TWO_PROJECTS_JSON <- jsonlite::read_json("data/projects-list.json")

# mock_token is an expired token as returned by zoltar. decoded contents:
# - header:  {"typ": "JWT", "alg": "HS256"}
# - payload: {"user_id": 3, "username": "model_owner1", "exp": 1558442805, "email": ""}
# - expiration:
#   05/21/2019 @ 12:46pm               (UTC)
#   2019-05-21T12:46:45+00:00          (ISO 8601)
#   Tuesday, May 21, 2019 12:46:45 PM  (GMT)
#   datetime(2019, 5, 21, 12, 46, 45)  (python): datetime.utcfromtimestamp(1558442805)
MOCK_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyX2lkIjozLCJ1c2VybmFtZSI6Im1vZGVsX293bmVyMSIsImV4cCI6MTU1ODQ0MjgwNSwiZW1haWwiOiIifQ.o03V2RxkFpA5ThhRAidwDWCdcQNeJzr1wwFkOFKUI74"

mock_authenticate <- function(zoltar_connection, token = MOCK_TOKEN) {
  with_mock(
    "zoltr::get_token" = function(...) {
      token
    },
    zoltar_authenticate(zoltar_connection, "username", "password"))
}


#
# ---- utility tests ----
#

test_that("url_for_projects() returns a URL", {
  zoltar_connection <- new_connection("http://example.com")
  expect_equal(url_for_projects(zoltar_connection), "http://example.com/api/projects/")
})


test_that("url_for_token_auth() returns a URL", {
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
  expect_equal(zoltar_connection$session$token, MOCK_TOKEN)
})


test_that("is_token_expired() works for an expired and unexpired tokens", {
  # test an expired token
  zoltar_connection <- new_connection("http://example.com")
  mock_authenticate(zoltar_connection)  # default token (mock_token) is expired
  expect_true(is_token_expired(zoltar_connection$session))

  # construct and test an unexpired token
  token_split <- strsplit(MOCK_TOKEN, ".", fixed = TRUE)  # 3 parts: header, payload, and signature
  old_header <- token_split[[1]][1]
  old_signature <- token_split[[1]][3]

  ten_min_from_now <- round(unclass(Sys.time() + (60 * 10)))  # exclude decimal portion - throws off some JWT tools
  new_payload <- list(user_id = 3, username = "model_owner1", exp = ten_min_from_now, email = "")
  new_payload_json <- jsonlite::toJSON(new_payload, auto_unbox = TRUE)
  unexpired_token <- paste0(old_header, '.', base64url::base64_urlencode(new_payload_json), '.', old_signature)

  mock_authenticate(zoltar_connection, token = unexpired_token)
  expect_false(is_token_expired(zoltar_connection$session))
})


test_that("get_resource() calls re_authenticate_if_necessary()", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    webmockr::stub_request("get", uri = "http://example.com/api/model/0/") %>%
      to_return(
        body = list(),
        status = 200,
        headers = list('Content-Type' = 'application/json; charset=utf-8'))
    get_resource(zoltar_connection, "http://example.com/api/model/0/")
    expect_equal(length(mock_calls(m)), 1)
  })
})


test_that("delete_resource() calls re_authenticate_if_necessary()", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock()
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = m, {
    webmockr::stub_request("delete", uri = "http://example.com/api/model/0/") %>%
      to_return(
        body = list(),
        status = 200,
        headers = list('Content-Type' = 'application/json; charset=utf-8'))
    delete_resource(zoltar_connection, "http://example.com/api/model/0/")
    expect_equal(length(mock_calls(m)), 1)
  })
})


#
# ---- projects tests ----
#

test_that("projects() returns a data.frame", {
  zoltar_connection <- new_connection("http://example.com")
  m <- mock(TWO_PROJECTS_JSON)
  testthat::with_mock("zoltr::get_resource" = m, {
    the_projects <- projects(zoltar_connection)
    expect_is(the_projects, "data.frame")
    expect_equal(names(the_projects), c("id", "url", "owner_url", "public", "name", "description", "home_url",
                                        "core_data"))
    expect_equal(nrow(the_projects), 2)  # 2 projects
    expect_equal(ncol(the_projects), 8)
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/projects/")

    project_row <- the_projects[1,]
    exp_row <- data.frame(id = 1L, url = "http://example.com/api/project/1/",
                          owner_url = "http://example.com/api/user/2/", public = TRUE, name = "public project",
                          description = "d", home_url = "http://example.com", core_data = "http://example.com",
                          stringsAsFactors = FALSE)
    rownames(project_row) <- c()
    rownames(exp_row) <- c()
    expect_equal(project_row, exp_row)

    project_row <- the_projects[2,]
    exp_row <- data.frame(id = 2L, url = "http://example.com/api/project/2/",
                          owner_url = "http://example.com/api/user/2/", public = FALSE, name = "private project",
                          description = "", home_url = "", core_data = "", stringsAsFactors = FALSE)
    rownames(project_row) <- c()
    rownames(exp_row) <- c()
    expect_equal(project_row, exp_row)
  })
})


test_that("projects() can handle NULL owner in project JSON", {
  zoltar_connection <- new_connection("http://example.com")
  two_projects_json_no_owner_json <- TWO_PROJECTS_JSON
  two_projects_json_no_owner_json[[1]]$owner <- NULL
  m <- mock(two_projects_json_no_owner_json)
  testthat::with_mock("zoltr::get_resource" = m, {
    the_projects <- projects(zoltar_connection)
    project_row <- the_projects[1,]
    exp_row <- data.frame(id = 1L, url = "http://example.com/api/project/1/", owner_url = as.character(NA),
                          public = TRUE, name = "public project", description = "d", home_url = "http://example.com",
                          core_data = "http://example.com", stringsAsFactors = FALSE)
    rownames(project_row) <- c()
    rownames(exp_row) <- c()
    expect_equal(project_row, exp_row)
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
    load("data/get_token_response.rda")  # 'response' contains a 200 response from a sample `get_token()` call via `zoltar_authenticate()`
    response
  },
                      zoltar_authenticate(zoltar_connection, "username", "password"))
  expect_equal(called_args$url, "http://example.com/api-token-auth/")
  expect_equal(called_args$body$username, zoltar_connection$username)
  expect_equal(called_args$body$password, zoltar_connection$password)
})

