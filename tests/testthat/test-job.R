context("cdc")

library(jsonlite)
library(testthat)
library(zoltr)


test_that("job_data() is correct", {
  zoltar_connection <- new_connection("http://example.com")
  cdc_csv_file <- "data/20161023-KoTstable-20161109-small.cdc.csv"  # file doesn't matter
  exp_data_frame <- utils::read.csv(cdc_csv_file, stringsAsFactors = FALSE, colClasses='character')  # "NA" -> NA
  m <- mock(exp_data_frame)
  testthat::with_mock("zoltr::get_resource" = m, {
    act_data_frame <- job_data(zoltar_connection, "http://example.com/api/project/1/")
    expect_equal(length(mock_calls(m)), 1)
    expect_equal(mock_args(m)[[1]][[2]], "http://example.com/api/project/1/data/")
    expect_is(act_data_frame, "data.frame")
    expect_equal(act_data_frame, exp_data_frame)
  })
})
