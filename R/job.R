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
      "FAILED"
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
#' Downloads the data associated with a job that has an associated file, such as a query's results.
#'
#' @return A `data.frame` of Job's data. Full documentation at \url{https://docs.zoltardata.com/}.
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param job_url URL of a valid job in zoltar_connection that has a data file associated with it
#' @export
#' @examples \dontrun{
#'   the_job_data <- job_data(conn, "http://example.com/api/job/2/")
#' }
job_data <- function(zoltar_connection, job_url) {
  data_url <- paste0(job_url, 'data/')
  get_resource(zoltar_connection, data_url)
}
