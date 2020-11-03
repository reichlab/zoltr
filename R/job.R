#
# ---- Job functions ----
#

status_as_str <- function(status_int) {
  # to map status ints to strings, we simply index into a vector. recall status starts with zero
  status_names <-
    c(
      "PENDING",
      "CLOUD_FILE_UPLOADED",
      "QUEUED",
      "CLOUD_FILE_DOWNLOADED",
      "SUCCESS",
      "FAILED",
      "TIMEOUT"
    )
  status_names[status_int + 1]
}


#' Get a job's information
#'
#' Gets a job's information that can be used to track the job's progress. Jobs represent long-running
#' asynchronous activities like uploading a file (e.g., a forecast or truth) or running a query.
#'
#' @return A `list` of job information for the passed job_url. it has these names:
#'   id, url, status, user, created_at, updated_at, failure_message, input_json, output_json
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param job_url URL of a valid job in zoltar_connection
#' @export
#' @examples \dontrun{
#'   the_job_info <- job_info(conn, "http://example.com/api/job/2/")
#' }
job_info <- function(zoltar_connection, job_url) {
  job_json <- get_resource(zoltar_connection, job_url)
  job_json$status <- status_as_str(job_json$status)
  job_json$created_at <- as.Date(job_json$created_at)
  job_json$updated_at <- as.Date(job_json$updated_at)
  job_json
}


#' Get a new forecast upload's url
#'
#' A helper function for jobs representing file uploads. Returns the URL of a newly-uploaded forecast
#' from job_info.
#'
#' @return A URL of the new forecast
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param the_job_info a `list` object as returned by \code{\link{job_info}}
#' @export
#' @examples \dontrun{
#'   new_forecast_url <- job_info_forecast_url(conn, "http://example.com/api/job/2/")
#' }
job_info_forecast_url <- function(zoltar_connection, the_job_info) {
  if (is.null(the_job_info$output_json$forecast_pk)) {
    NULL
  } else {
    paste0(zoltar_connection$host, "/api/forecast/", the_job_info$output_json$forecast_pk, "/")
  }
}


#' Gets a job's file's data
#'
#' Downloads the data for jobs that have an associated file, such as a query's results. Called on Jobs
#' that are the results of a project forecast or score queries via `submit_query()`. NB: It is a 404 Not Found
#' error if this is called on a Job that has no underlying S3 data file, which can happen b/c: 1) 24 hours has
#' passed (the expiration time) or 2) the Job is not complete and therefore has not saved the data file. For
#' the latter you may use `busy_poll_job()` to ensure the job is done.
#'
#' @return A `data.frame` of Job's data. The columns depend on is_forecast_query. Full documentation at
#'   \url{https://docs.zoltardata.com/}.
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param job_url URL of a valid job in zoltar_connection that has a data file associated with it
#' @param is_forecast_query boolean indicating whether this is to query forecasts or scores
#' @export
#' @examples \dontrun{
#'   the_job_data <- job_data(conn, "http://example.com/api/job/2/")
#' }
job_data <- function(zoltar_connection, job_url, is_forecast_query) {
  data_url <- paste0(job_url, 'data/')
  # columns and expected types depend on is_forecast_query:
  #
  # `is_forecast_query = TRUE`:
  #   model,timezero,season,unit,target,class,value,cat,prob,sample,quantile,family,param1,param2,param3
  #   c     D        c      c    c      c     ?     ?   ?    ?      ?        ?      ?      ?      ?
  #
  # `is_forecast_query = FALSE`: recall that the first six columns are fixed, but the number of score ones varies:
  #   model,timezero,season,unit,target,truth,score_1,score_2,...
  #   c     c        c      c    c      ?     ?       ?       ...
  score_cols <- readr::cols(
    .default = readr::col_double(),  # scores
    model = readr::col_character(),
    timezero = readr::col_date(format = ""),
    season = readr::col_character(),
    unit = readr::col_character(),
    target = readr::col_character()
  )
  get_resource(zoltar_connection, data_url, if (is_forecast_query) "cDcccc????d????" else score_cols)
}


#' Poll job's status
#'
#' A convenience function that polls the passed Job's status waiting for either FAILED, TIMEOUT, or SUCCESS.
#'
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param job_url URL of a valid job in zoltar_connection
#' @param verbose if TRUE, print messages on job status poll
#' @export
#' @examples \dontrun{
#'   busy_poll_job(conn, "http://example.com/api/job/2/")
#' }
busy_poll_job <- function(zoltar_connection, job_url, verbose = TRUE) {
  if (verbose) {
    cat(paste0("polling for status change. job_url=", job_url, "\n"))
  }
  while (TRUE) {
    the_job_info <- job_info(zoltar_connection, job_url)
    if (verbose) {
      cat(paste0(the_job_info$status, "\n"))
    }
    if ((the_job_info$status == "FAILED") || (the_job_info$status == "TIMEOUT")) {
      stop(paste0("job failed or timed out: status=", the_job_info$status, ", job_url=", job_url, ", failure_message='",
                  the_job_info$failure_message, "'"),
           call. = FALSE)
    }
    if (the_job_info$status == "SUCCESS") {
      break
    }
    Sys.sleep(1)
  }
}
