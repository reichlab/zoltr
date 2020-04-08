#
# ---- forecast functions ----
#

#' Gets a forecast's information
#'
#' @return A `list` of forecast information for the passed forecast_url
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param forecast_url URL of a forecast in zoltar_connection's forecasts
#' @export
#' @examples \dontrun{
#'   the_forecast_info <- forecast_info(conn, "http://example.com/api/forecast/1/")
#' }
forecast_info <- function(zoltar_connection, forecast_url) {
  the_forecast_info <- get_resource(zoltar_connection, forecast_url)
  the_forecast_info$forecast_model_url <- the_forecast_info$forecast_model
  the_forecast_info$forecast_model <- NULL
  the_forecast_info$created_at <- as.Date(the_forecast_info$created_at)  # "2020-03-05T15:47:47.369231-05:00"
  the_forecast_info$time_zero$timezero_date <- as.Date(the_forecast_info$time_zero$timezero_date)
  the_forecast_info$time_zero$data_version_date <- if (is.null(the_forecast_info$time_zero$data_version_date)) NA
    else as.Date(the_forecast_info$time_zero$data_version_date)
  the_forecast_info
}


#' Delete a forecast
#'
#' Deletes the forecast with the passed URL. This is permanent and cannot be undone.
#'
#' @return None
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param forecast_url URL of a forecast in zoltar_connection's forecasts
#' @export
#' @examples \dontrun{
#'   delete_forecast(conn, "http://example.com/api/forecast/1/")
#' }
delete_forecast <- function(zoltar_connection, forecast_url) {
  delete_resource(zoltar_connection, forecast_url)
}


#' Gets a forecast's data
#'
#' @return Forecast data as a `list` in the Zoltar standard format. meta information is ignored. Full documentation at
#'   \url{https://docs.zoltardata.com/}.
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param forecast_url URL of a forecast in zoltar_connection's forecasts
#' @export
#' @examples \dontrun{
#'   forecast_data <- download_forecast(conn, "http://example.com/api/forecast/1/")
#' }
download_forecast <- function(zoltar_connection, forecast_url) {
  forecast_data_url <- paste0(forecast_url, 'data/')
  forecast_data <- get_resource(zoltar_connection, forecast_data_url)
  if (is.null(forecast_data)) {  # true for tests
    return(NULL)
  }

  # convert date-based targets to Date objects
  targets_df <- data_frame_from_targets_json(forecast_data$meta$targets)
  date_target_names <- targets_df[targets_df$type == "date", "name"]
  if (length(date_target_names) != 0) {
    for (prediction_element_idx in seq_along(forecast_data$predictions)) {
      prediction_element <- forecast_data$predictions[[prediction_element_idx]]
      is_date_prediction <- prediction_element$target %in% date_target_names
      if (!is_date_prediction) {
        next
      }

      if (prediction_element$class == "point") {
        point_value <- as.Date(forecast_data$predictions[[prediction_element_idx]]$prediction$value,
                               format = YYYY_MM_DD_DATE_FORMAT)
        forecast_data$predictions[[prediction_element_idx]]$prediction$value <- point_value
      } else if (prediction_element$class == "bin") {
        cat_value <- lapply(forecast_data$predictions[[prediction_element_idx]]$prediction$cat,
                            FUN = function(x) as.Date(x, YYYY_MM_DD_DATE_FORMAT))
        forecast_data$predictions[[prediction_element_idx]]$prediction$cat <- cat_value
      } else if (prediction_element$class == "sample") {
        sample_value <- lapply(forecast_data$predictions[[prediction_element_idx]]$prediction$sample,
                               FUN = function(x) as.Date(x, YYYY_MM_DD_DATE_FORMAT))
        forecast_data$predictions[[prediction_element_idx]]$prediction$sample <- sample_value
      } # else "named"
    }
  }

  forecast_data
}
