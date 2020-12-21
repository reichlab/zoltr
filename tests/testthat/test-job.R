context("cdc")

library(jsonlite)
library(testthat)
library(zoltr)


test_that("job_data() calls are correct", {
  zoltar_connection <- new_connection("http://example.com")
  cdc_file <- "data/20161023-KoTstable-20161109-small.cdc.csv"  # file doesn't matter for this test, but does below
  exp_data_frame <- utils::read.csv(cdc_file, stringsAsFactors = FALSE, colClasses = 'character')  # "NA" -> NA
  mock_reauthenticate <- mock(exp_data_frame)
  testthat::with_mock("zoltr::get_resource" = mock_reauthenticate, {
    act_data_frame <- job_data(zoltar_connection, "http://example.com/api/job/2/", "forecasts")
    expect_equal(length(mock_calls(mock_reauthenticate)), 1)
    expect_equal(mock_args(mock_reauthenticate)[[1]][[2]], "http://example.com/api/job/2/data/")
    expect_is(act_data_frame, "data.frame")
    expect_equal(act_data_frame, exp_data_frame)
  })
})


test_that("job_data() column types are correct for forecast CSV data", {  # JSON data tested many other places
  zoltar_connection <- new_connection("http://example.com")
  mock_reauthenticate <- mock()
  mock_from_json <- mock()
  mock_read_csv <- mock()
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = mock_reauthenticate, {
    testthat::with_mock("jsonlite::fromJSON" = mock_from_json, {
      testthat::with_mock("readr::read_csv" = mock_read_csv, {
        webmockr::stub_request("get", uri = "http://example.com/api/job/2/data/") %>%
          to_return(
            status = 200,
            headers = list('Content-Type' = 'text/csv'))
        job_data(zoltar_connection, "http://example.com/api/job/2/", "forecasts")
        expect_equal(length(mock_calls(mock_from_json)), 0)
        expect_equal(length(mock_calls(mock_read_csv)), 1)
        expect_equal(mock_args(mock_read_csv)[[1]]$col_types, "cDcccc????d????")
        # note: we do not test the column types of the actual data returned by job_data b/c we trust that
        # readr::read_csv correctly applies col_types
      })
    })
  })
})


test_that("job_data() column types are correct for truth CSV data", {  # JSON data tested many other places
  zoltar_connection <- new_connection("http://example.com")
  mock_reauthenticate <- mock()
  mock_from_json <- mock()
  mock_read_csv <- mock()
  testthat::with_mock("zoltr::re_authenticate_if_necessary" = mock_reauthenticate, {
    testthat::with_mock("jsonlite::fromJSON" = mock_from_json, {
      testthat::with_mock("readr::read_csv" = mock_read_csv, {
        webmockr::stub_request("get", uri = "http://example.com/api/job/2/data/") %>%
          to_return(
            status = 200,
            headers = list('Content-Type' = 'text/csv'))
        job_data(zoltar_connection, "http://example.com/api/job/2/", "truth")
        expect_equal(length(mock_calls(mock_from_json)), 0)
        expect_equal(length(mock_calls(mock_read_csv)), 1)
        expect_equal(mock_args(mock_read_csv)[[1]]$col_types, "Dcc?")
        # note: we do not test the column types of the actual data returned by job_data b/c we trust that
        # readr::read_csv correctly applies col_types
      })
    })
  })
})
