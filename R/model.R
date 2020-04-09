#
# ---- model functions ----
#

YYYY_MM_DD_DATE_FORMAT <- "%Y-%m-%d"  # e.g., '2017-01-17'


#' Get information about a model
#'
#' @return A `list` of model information for the passed model_url
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param model_url URL of a model in zoltar_connection's models
#' @export
#' @examples \dontrun{
#'   the_model_info <- model_info(conn, "http://www.zoltardata.com/api/model/1/")
#' }
model_info <- function(zoltar_connection, model_url) {
  the_model_info <- get_resource(zoltar_connection, model_url)
  the_model_info$aux_data_url <- if (is.null(the_model_info$aux_data_url)) NA else the_model_info$aux_data_url
  the_model_info$forecasts <- NULL  # obtained via forecasts(zoltar_connection, model_url)
  the_model_info
}


#' Create a model
#'
#' Creates the model in the passed project using the passed list. Fails if a model with the passed name already exists.
#'
#' @return model_url of the newly-created model
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_url url of a project in zoltar_connection's projects. this is the project the new model will be created
#'   in
#' @param model_config A `list` containing a Zoltar model configuration. An example: example-model-config.json .
#'   Full documentation at \url{https://docs.zoltardata.com/}.
#' @export
#' @examples \dontrun{
#'   new_model_url <- create_model(conn, "https://www.zoltardata.com/project/9/",
#'                      jsonlite::read_json("example-model-config.json"))
#' }
create_model <- function(zoltar_connection, project_url, model_config) {
  re_authenticate_if_necessary(zoltar_connection)
  models_url <- paste0(project_url, 'models/')
  response <- httr::POST(
    url = models_url,
    add_auth_headers(zoltar_connection),
    body = list(model_config = model_config),
    encode = "json")
  # the Zoltar API returns 400 if there was an error POSTing. the content is JSON with a $error key that contains the
  # error message
  json_response <- httr::content(response, "parsed")
  if (response$status_code == 400) {
    stop(json_response$error, call. = FALSE)
  }

  json_response$url  # throw away rest of json and let model_info() reload/refresh it
}


#' Delete a model
#'
#' Deletes the model with the passed ID. This is permanent and cannot be undone.
#'
#' @return None
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param model_url URL of a model in zoltar_connection's models
#' @export
#' @examples \dontrun{
#'   delete_model(conn, "http://www.zoltardata.com/api/model/1/")
#' }
delete_model <- function(zoltar_connection, model_url) {
  delete_resource(zoltar_connection, model_url)
}


#' Get a model's forecasts
#'
#' @return A `data.frame` of forecast information for the passed model
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param model_url URL of a model in zoltar_connection's models
#' @export
#' @examples \dontrun{
#'   the_forecasts <- forecasts(conn, "http://www.zoltardata.com/api/model/1/")
#' }
forecasts <- function(zoltar_connection, model_url) {
  forecasts_url <- paste0(model_url, 'forecasts/')
  forecasts_json <- get_resource(zoltar_connection, forecasts_url)
  id_column <- c()                  # integer
  url_column <- c()                 # character
  forecast_model_url_column <- c()  # ""
  source_column <- c()              # ""
  timezero_url_column <- c()        # ""
  created_at_column <- c()          # Date
  forecast_data_url_column <- c()   # character
  for (forecast_json in forecasts_json) {
    id_column <- append(id_column, forecast_json$id)
    url_column <- append(url_column, forecast_json$url)
    forecast_model_url_column <- append(forecast_model_url_column, forecast_json$forecast_model)
    source_column <- append(source_column, forecast_json$source)
    timezero_url_column <- append(timezero_url_column, forecast_json$time_zero$url)  # "unnest" timezeros to URL
    created_at_column <- append(created_at_column, as.Date(forecast_json$created_at))  # "2020-03-05T15:47:47.369231-05:00"
    forecast_data_url_column <- append(forecast_data_url_column, forecast_json$forecast_data)
  }
  data.frame(id = id_column, url = url_column, forecast_model_url = forecast_model_url_column, source = source_column,
             timezero_url = timezero_url_column, created_at = created_at_column,
             forecast_data_url = forecast_data_url_column, stringsAsFactors = FALSE)
}


#' Upload a forecast
#'
#' This function submits forecast data to the server for uploading. Returns an UploadFileJob object that can be used to
# 'track the upload's progress. (Uploads are processed in a queue, which means they are delayed until their turn comes
#' up, which depends on the number of current uploads in the queue. Zoltar tracks these via `UploadFileJob` objects.)
#'
#' @return An UploadFileJob URL for the upload
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param model_url URL of a model in zoltar_connection's projects
#' @param timezero_date The date of the project timezero you are uploading for. it is a string in format YYYYMMDD
#' @param forecast_data Forecast data as a `list` in the Zoltar standard format
#' @export
#' @examples \dontrun{
#'   forecast_data <- jsonlite::read_json("docs-predictions.json")
#'   upload_file_job_url <- upload_forecast(conn, "http://www.zoltardata.com/api/model/1/",
#'                                          "2017-01-17", forecast_data)
#' }
upload_forecast <- function(zoltar_connection, model_url, timezero_date, forecast_data) {
  if (!(inherits(forecast_data, "list"))) {
    stop("forecast_data was not a `list`", call. = FALSE)
  }

  re_authenticate_if_necessary(zoltar_connection)
  forecasts_url <- paste0(model_url, 'forecasts/')
  message(paste0("upload_forecast(): POST: ", forecasts_url))
  temp_json_file <- tempfile(pattern = "forecast", fileext = ".json")

  # w/out auto_unbox: primitives are written as lists of one item, e.g.,
  # {"unit":["HHS Region 1"], "target":["1 wk ahead"], "class":["bin"], "prediction":{"cat":[[0] ,[0.1]],"prob":[[0.1], [0.9]]}}
  jsonlite::write_json(forecast_data, temp_json_file, auto_unbox = TRUE)

  response <- httr::POST(
    url = forecasts_url,
    httr::accept_json(),
    add_auth_headers(zoltar_connection),
    body = list(data_file = httr::upload_file(temp_json_file), timezero_date = timezero_date))
  # the Zoltar API returns 400 if there was an error POSTing. the content is JSON with a $error key that contains the
  # error message
  json_response <- httr::content(response, "parsed")
  if (response$status_code == 400) {
    stop(json_response$error, call. = FALSE)
  }

  json_response$url  # throw away rest of json and let upload_file_job_info() reload/refresh it
}
