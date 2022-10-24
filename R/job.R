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
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @param job_url URL of a valid job in zoltar_connection
#' @export
#' @examples \dontrun{
#'   the_job_info <- job_info(conn, "http://example.com/api/job/2/")
#' }
job_info <- function(zoltar_connection, job_url) {
  job_json <- get_resource(zoltar_connection, job_url)
  job_json$status <- status_as_str(job_json$status)
  job_json$created_at <- lubridate::parse_date_time(job_json$created_at, DATE_TIME_TZ_FORMAT, exact = TRUE)
  job_json$updated_at <- lubridate::parse_date_time(job_json$updated_at, DATE_TIME_TZ_FORMAT, exact = TRUE)

  # convert output_json$missing_time_zeros to dates if present (only present for jobs with "type"="UPLOAD_TRUTH")
  if (!is.null(job_json$output_json$missing_time_zeros)) {
    job_json$output_json$missing_time_zeros <- lapply(job_json$output_json$missing_time_zeros,
                                                      FUN = function(x) as.Date(x, YYYY_MM_DD_DATE_FORMAT))
  }
  job_json
}


#' Get a new forecast upload's url
#'
#' A helper function for jobs representing file uploads. Returns the URL of a newly-uploaded forecast
#' from job_info.
#'
#' @return A URL of the new forecast
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @param the_job_info a `list` object as returned by [job_info()]
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
#' that are the results of a project forecast or truth queries via [submit_query()]. NB: It is a 404 Not Found
#' error if this is called on a Job that has no underlying S3 data file, which can happen b/c: 1) 24 hours has
#' passed (the expiration time) or 2) the Job is not complete and therefore has not saved the data file. For
#' the latter you may use [busy_poll_job()] to ensure the job is done.
#'
#' @return A `data.frame` of Job's data. The columns depend on query_type - see
#'   <https://docs.zoltardata.com/fileformats/#truth-data-format-csv> and
#'   <https://docs.zoltardata.com/fileformats/#forecast-data-format-csv>.
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @param job_url URL of a valid job in zoltar_connection that has a data file associated with it
#' @param query_type A character indicating the type of query to run. Must be one of: "forecasts" or "truth".
#' @export
#' @examples \dontrun{
#'   the_job_data <- job_data(conn, "http://example.com/api/job/2/")
#' }
job_data <- function(zoltar_connection, job_url, query_type) {
  if (!query_type %in% c("forecasts", "truth")) {
    stop("invalid query_type: '", query_type, "'", call. = FALSE)
  }

  data_url <- paste0(job_url, 'data/')
  # columns and expected types depend on query_type:
  #
  # query_type == "forecasts": recall all but the first six columns have types that depend on the target, and can be
  #                            number, character, Date, or logical, so we use '?':
  #   model,timezero,season,unit,target,class,value,cat,prob,sample,quantile,family,param1,param2,param3
  #   c     D        c      c    c      c     ?     ?   ?    ?      ?        ?      ?      ?      ?
  #
  # query_type == "truth": recall that the value column's type depend on the target, and can be number, character, Date,
  #                        or logical, so we use '?':
  #   timezero,unit,target,value
  #   D        c    c      ?
  if (query_type == "forecasts") {
    col_types <- "cDcccc????d????"
  } else {  # query_type == "truth"
    col_types <- "Dcc?"
  }
  get_resource(zoltar_connection, data_url, col_types)
}


#' Poll job's status
#'
#' A convenience function that polls the passed Job's status waiting for either FAILED, TIMEOUT, or SUCCESS.
#'
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @param job_url URL of a valid job in zoltar_connection
#' @param verbose if TRUE, print messages on job status poll
#' @export
#' @examples \dontrun{
#'   busy_poll_job(conn, "http://example.com/api/job/2/")
#' }
busy_poll_job <- function(zoltar_connection, job_url, verbose = TRUE) {
  if (verbose) {
    message("polling for status change. job_url=", job_url)
  }
  while (TRUE) {
    the_job_info <- job_info(zoltar_connection, job_url)
    if (verbose) {
      message(the_job_info$status)
    }
    if ((the_job_info$status == "FAILED") || (the_job_info$status == "TIMEOUT")) {
      stop("job failed or timed out: status=", the_job_info$status,
           ", job_url=", job_url,
           ", failure_message='", the_job_info$failure_message, "'",
           call. = FALSE)
    }
    if (the_job_info$status == "SUCCESS") {
      break
    }
    Sys.sleep(1)
  }
}
