#
# ---- UploadFileJob functions ----
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


#' Get an upload's information
#'
#' Gets an upload's information that can be used to track the upload's progress. (Uploads are processed in a queue,
#  which means they are delayed until their turn comes up, which depends on the number of current uploads in the queue.)
#'
#' @return A `list` of upload information for the passed upload_file_job_url. it has these names:
#'   id, url, status, user, created_at, updated_at, failure_message, filename, input_json, output_json
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param upload_file_job_url URL of a job in zoltar_connection that was uploaded via \code{\link{upload_forecast}}
#' @export
#' @examples \dontrun{
#'   the_upload_info <- upload_info(conn, "http://example.com/api/uploadfilejob/2/")
#' }
upload_info <- function(zoltar_connection, upload_file_job_url) {
  ufj_json <- get_resource(zoltar_connection, upload_file_job_url)
  ufj_json$status <- status_as_str(ufj_json$status)
  ufj_json$created_at <- as.Date(ufj_json$created_at)
  ufj_json$updated_at <- as.Date(ufj_json$updated_at)
  ufj_json
}


#' Get a new forecast upload's url
#'
#' A helper function that returns the URL of a newly-uploaded forecast from upload_info.
#'
#' @return A URL of the new forecast
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param the_upload_info a `list` object as returned by \code{\link{upload_info}}
#' @export
#' @examples \dontrun{
#'   new_forecast_url <- upload_info_forecast_url(conn, "http://example.com/api/uploadfilejob/2/")
#' }
upload_info_forecast_url <- function(zoltar_connection, the_upload_info) {
  if (is.null(the_upload_info$output_json$forecast_pk)) {
    NULL
  } else {
    paste0(zoltar_connection$host, "/api/forecast/", the_upload_info$output_json$forecast_pk, "/")
  }
}
