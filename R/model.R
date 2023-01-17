#
# ---- model functions ----
#

DATE_TIME_TZ_FORMAT <- "%Y-%m-%dT%H:%M:%OS%z"  # e.g., '2020-04-13 14:27:27 UTC'


#' Get information about a model
#'
#' @return A `list` of model information for the passed model_url
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
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
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @param project_url url of a project in zoltar_connection's projects. this is the project the new model will be created
#'   in
#' @param model_config A `list` containing a Zoltar model configuration. An example: example-model-config.json .
#'   Full documentation at <https://docs.zoltardata.com/>.
#' @export
#' @examples \dontrun{
#'   new_model_url <- create_model(conn, "https://www.zoltardata.com/api/project/9/",
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
    stop("POST status was not 400. status_code=", response$status_code, ", json_response=", json_response,
         call. = FALSE)
  }

  json_response$url  # throw away rest of the model json and let model_info() reload/refresh it
}


#' Edit a model
#'
#' Edits the model in the passed project using the passed list. Fails if a model with the passed name already exists.
#'
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @param model_url url of a project in zoltar_connection's projects. this is the project the new model will be edited
#'   in
#' @param model_config A `list` containing a Zoltar model configuration. An example: example-model-config.json .
#'   Full documentation at <https://docs.zoltardata.com/>.
#' @export
#' @examples \dontrun{
#'   edit_model(conn, "https://www.zoltardata.com/api/model/2/",
#'     jsonlite::read_json("example-model-config.json"))
#' }
edit_model <- function(zoltar_connection, model_url, model_config) {
  re_authenticate_if_necessary(zoltar_connection)
  response <- httr::PUT(
    url = model_url,
    add_auth_headers(zoltar_connection),
    body = list(model_config = model_config),
    encode = "json")
  # the Zoltar API returns 400 if there was an error PUTting. the content is JSON with a $error key that contains the
  # error message
  json_response <- httr::content(response, "parsed")
  if (response$status_code == 400) {
    stop("PUT status was not 400. status_code=", response$status_code, ", json_response=", json_response,
         call. = FALSE)
  }
}


#' Delete a model
#'
#' Deletes the model with the passed ID. This is permanent and cannot be undone.
#'
#' @return None
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
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
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @param model_url URL of a model in zoltar_connection's models
#' @export
#' @examples \dontrun{
#'   the_forecasts <- forecasts(conn, "http://www.zoltardata.com/api/model/1/")
#' }
forecasts <- function(zoltar_connection, model_url) {
  forecasts_url <- paste0(model_url, 'forecasts/')
  forecasts_json <- get_resource(zoltar_connection, forecasts_url)
  id_column <- c()                          # integer
  url_column <- c()                         # character
  forecast_model_url_column <- c()          # ""
  source_column <- c()                      # ""
  timezero_url_column <- c()                # ""
  timezero_date_column <- c()      # Date
  data_version_date_column <- c()  # ""
  is_season_start_column <- c()    # logical
  created_at_column <- c()                  # Date
  issued_at_column <- c()                  # Date
  notes_column <- c()                       # character
  forecast_data_url_column <- c()           # ""
  for (forecast_json in forecasts_json) {
    id_column <- append(id_column, forecast_json$id)
    url_column <- append(url_column, forecast_json$url)
    forecast_model_url_column <- append(forecast_model_url_column, forecast_json$forecast_model)
    source_column <- append(source_column, forecast_json$source)

    # "unnest" timezero information:
    timezero_url_column <- append(timezero_url_column, forecast_json$time_zero$url)
    timezero_date_column <- append(timezero_date_column,
                                   if (is.null(forecast_json$time_zero$timezero_date)) as.Date(NA)
                                   else as.Date(forecast_json$time_zero$timezero_date))
    data_version_date_column <- append(data_version_date_column,
                                       if (is.null(forecast_json$time_zero$data_version_date)) as.Date(NA)
                                       else as.Date(forecast_json$time_zero$data_version_date))
    is_season_start_column <- append(is_season_start_column, forecast_json$time_zero$is_season_start)
    created_at_column <- append(created_at_column,
                                lubridate::parse_date_time(
                                  forecast_json$created_at, DATE_TIME_TZ_FORMAT, exact = TRUE))  # e.g., "2020-03-05T15:47:47.369231-05:00"
    issued_at_column <- append(issued_at_column,
                               lubridate::parse_date_time(
                                 forecast_json$issued_at, DATE_TIME_TZ_FORMAT, exact = TRUE))  # ""
    notes_column <- append(notes_column, if (is.null(forecast_json$notes)) as.character(NA) else forecast_json$notes)
    forecast_data_url_column <- append(forecast_data_url_column, forecast_json$forecast_data)
  }
  data.frame(id = id_column, url = url_column, forecast_model_url = forecast_model_url_column, source = source_column,
             timezero_url = timezero_url_column, timezero_date = timezero_date_column,
             data_version_date = data_version_date_column,
             is_season_start = is_season_start_column, created_at = created_at_column, issued_at = issued_at_column,
             notes = notes_column, forecast_data_url = forecast_data_url_column, stringsAsFactors = FALSE)
}


#' Upload a forecast
#'
#' This function submits forecast data to the server for uploading. Returns a Job object that can be used to
# 'track the upload's progress. (Uploads are processed in a queue, which means they are delayed until their turn comes
#' up, which depends on the number of current uploads in the queue. Zoltar tracks these via `Job` objects.)
#'
#' @return A Job URL for the upload
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @param model_url URL of a model in zoltar_connection's projects
#' @param timezero_date The date of the project timezero you are uploading for. it is a string in format YYYYMMDD
#' @param forecast_data Forecast data to upload data to upload, either a `list` (if is_json==TRUE) or a `dataframe`
#'   otherwise. formats are documented at https://docs.zoltardata.com/
#' @param is_json TRUE if forecast_data is JSON (list) format, and FALSE if it is CSV (dataframe) format
#' @param notes Optional user notes for the new forecast
#' @export
#' @examples \dontrun{
#'   forecast_data <- jsonlite::read_json("docs-predictions.json")
#'   job_url <- upload_forecast(conn, "http://www.zoltardata.com/api/model/1/",
#'                              "2017-01-17", forecast_data, TRUE, "a mid-January forecast")
#' }
upload_forecast <- function(zoltar_connection, model_url, timezero_date, forecast_data, is_json = TRUE, notes = "") {
  # validate forecast_data and is_json combination. there are two valid cases:
  # - forecast_data is a `list` and is_json == TRUE (Zoltar JSON format)
  # - forecast_data is a `dataframe` and is_json == FALSE (Zoltar CSV format)
  if ((!(inherits(forecast_data, "list")) && is_json) ||
    (!(inherits(forecast_data, "data.frame")) && !is_json)) {
    stop("invalid forecast_data type for is_json", call. = FALSE)
  }

  re_authenticate_if_necessary(zoltar_connection)
  forecasts_url <- paste0(model_url, 'forecasts/')
  message("upload_forecast(): POST: ", forecasts_url)
  temp_data_file <- tempfile(pattern = "forecast-", fileext = if (is_json) '.json' else '.csv')

  # NB re: jsonlite::write_json(): w/out auto_unbox: primitives are written as lists of one item, e.g.,
  #   {"unit":["HHS Region 1"], "target":["1 wk ahead"], "class":["bin"], "prediction":{"cat":[[0] ,[0.1]],"prob":[[0.1], [0.9]]}}
  # w/out digits: some small numbers are converted to zero - see [Inconsistent treatment of digits across 1e-05 #184] -
  #   https://github.com/jeroen/jsonlite/issues/184
  if (is_json) {
    jsonlite::write_json(forecast_data, temp_data_file, auto_unbox = TRUE, digits = NA)
  } else {
    utils::write.table(forecast_data, temp_data_file, col.names = TRUE, row.names = FALSE, sep = ",", na = '',
                       quote = FALSE)
  }

  response <- httr::POST(
    url = forecasts_url,
    httr::accept_json(),
    add_auth_headers(zoltar_connection),
    body = list(data_file = httr::upload_file(temp_data_file), format = if (is_json) 'json' else 'csv',
                timezero_date = timezero_date, notes = notes))
  # the Zoltar API returns 400 if there was an error POSTing. the content is JSON with a $error key that contains the
  # error message
  json_response <- httr::content(response, "parsed")
  if (response$status_code == 400) {
    stop("POST status was not 400. status_code=", response$status_code, ", json_response=", json_response,
         call. = FALSE)
  }

  json_response$url  # throw away rest of job json and let job_info() reload/refresh it
}
