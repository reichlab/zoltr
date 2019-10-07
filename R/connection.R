library(base64url)
library(dplyr)
library(httr)
library(jsonlite)
library(readr)  # apparently required by httr


#
# ---- utility functions ----
#

# returns the trailing integer id from a url structured like: "http://example.com/api/forecast/71/" -> 71L
id_for_url <- function(url) {
  url_split <- strsplit(url, "/")
  first_split <- url_split[[1]]
  id_str <- first_split[length(first_split)]
  as.integer(id_str)
}


# returns an API URL for the passed project_id, sans trailing slash
url_for_project_id <- function(zoltar_connection, project_id) {
  paste0(zoltar_connection$host, '/api/project/', project_id)
}


url_for_model_id <- function(zoltar_connection, model_id) {
  paste0(zoltar_connection$host, '/api/model/', model_id)
}


url_for_model_forecasts_id <- function(zoltar_connection, model_id) {
  paste0(url_for_model_id(zoltar_connection, model_id), '/forecasts/')
}


url_for_upload_file_job_id <- function(zoltar_connection, upload_file_job_id) {
  paste0(zoltar_connection$host, '/api/uploadfilejob/', upload_file_job_id)
}


url_for_forecast_id <- function(zoltar_connection, forecast_id) {
  paste0(zoltar_connection$host, '/api/forecast/', forecast_id)
}


url_for_forecast_data_id <- function(zoltar_connection, forecast_id) {
  paste0(url_for_forecast_id(zoltar_connection, forecast_id), '/data/')
}


url_for_token_auth <- function(zoltar_connection) {
  paste0(zoltar_connection$host, '/api-token-auth/')
}


add_auth_headers <- function(zoltar_connection) {
  if (inherits(zoltar_connection$session, "ZoltarSession")) {
    httr::add_headers("Authorization"=paste0("JWT ", zoltar_connection$session$token))
  }
}


# deletes the resource at the passed URL
delete_resource <- function(zoltar_connection, url) {
  re_authenticate_if_necessary(zoltar_connection)
  message(paste0("delete_resource(): DELETE: ", url))
  response <- httr::DELETE(url=url, add_auth_headers(zoltar_connection))
  httr::stop_for_status(response)
}


#
# ---- ZoltarConnection class ----
#

#' Get a connection to a Zoltar host
#'
#' Returns a new connection object, which is the starting point for working with the Zoltar API. Once you have the
#' connection you can call \code{\link{zoltar_authenticate}} on it, and then call \code{\link{projects}} to get a list
#' of Project objects to start working with.
#'
#' @return A `ZoltarConnection` object
#' @param host The Zoltar site to connect to. Defaults to \url{https://zoltardata.com}
#' @export
#' @examples \dontrun{
#'   conn <- new_connection()
#' }

new_connection <- function(host="https://zoltardata.com") {
  self <- structure(environment(), class="ZoltarConnection")
  host <- host
  username <- NULL
  password <- NULL
  session <- NULL
  self
}


#' @export
print.ZoltarConnection <-
  function(x, ...) {
    cat(class(x), " '", x$host, "' ",
      if (is.null(x$session)) "not authenticated"
      else paste0("authenticated (exp=", token_expiration_date(x$session), " UTC)"),
      "\n", sep='')
  }


#' Log in to a Zoltar host
#'
#' Returns a new `ZoltarConnection` object, which is the starting point for working with the Zoltar API.
#' Once you have the connection you can call zoltar_authenticate() on it, and call projects() to get a list of objects
#' to start working with.
#'
#' @return None
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}.
#' @param username Username for the account to use on the connection's host
#' @param password Password ""
#' @export
#' @examples \dontrun{
#'   zoltar_authenticate(conn, "USERNAME", "PASSWORD")
#' }
zoltar_authenticate <- function(zoltar_connection, username, password) {
  zoltar_connection$username <- username
  zoltar_connection$password <- password
  zoltar_connection$session <- new_session(zoltar_connection)
  }


re_authenticate_if_necessary <- function(zoltar_connection) {
  if (inherits(zoltar_connection$session, "ZoltarSession") && is_token_expired(zoltar_connection$session)) {
    message(paste0("re-authenticating expired token '", zoltar_connection$host, "'"))
    zoltar_authenticate(zoltar_connection, zoltar_connection$username, zoltar_connection$password)
  }
}


get_resource <- function(zoltar_connection, url) {
  re_authenticate_if_necessary(zoltar_connection)
  message(paste0("get_resource(): GET: ", url))
  response <- httr::GET(url=url, add_auth_headers(zoltar_connection))
  httr::stop_for_status(response)
  httr::content(response, as="parsed", encoding="UTF-8")
}


#' Get information about all projects
#'
#' @return A `data.frame` of all projects' contents
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @export
#' @examples \dontrun{
#'   the_projects <- projects(conn)
#' }
projects <- function(zoltar_connection) {
  projects_json <- get_resource(zoltar_connection, paste0(zoltar_connection$host, '/api/projects/'))
  id_column <- c()
  url_column <- c()
  owner_id_column <- c()
  is_public_column <- c()
  name_column <- c()
  description_column <- c()
  home_url_column <- c()
  core_data_column <- c()
  for (project_json in projects_json) {
    id_column <- append(id_column, project_json$id)
    url_column <- append(url_column, project_json$url)

    owner_id_value <- if (is.null(project_json$owner)) NA else id_for_url(project_json$owner)
    owner_id_column <- append(owner_id_column, owner_id_value)

    is_public_column <- append(is_public_column, project_json$is_public)
    name_column <- append(name_column, project_json$name)
    description_column <- append(description_column, project_json$description)
    home_url_column <- append(home_url_column, project_json$home_url)
    core_data_column <- append(core_data_column, project_json$core_data)
  }
  data.frame(id=id_column, url=url_column, owner_id=owner_id_column, public=is_public_column, name=name_column,
    description=description_column, home_url=home_url_column, core_data=core_data_column,
    stringsAsFactors=FALSE)
}


#
# ---- project functions ----
#

#' Get information about a project
#'
#' @return A `list` of project information for the passed project_id
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_id ID of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_project_info <- project_info(conn, 4L)
#' }
project_info <- function(zoltar_connection, project_id) {
  get_resource(zoltar_connection, url_for_project_id(zoltar_connection, project_id))
}


#' Get a project's scores
#'
#' @return A `data.frame` of score data for all models in the passed project ID
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_id ID of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_scores <- scores(conn, 4L)
#' }
scores <- function(zoltar_connection, project_id) {
  scores_url <- paste0(url_for_project_id(zoltar_connection, project_id), '/score_data/')
  get_resource(zoltar_connection, scores_url)
}


#' Get a project's models
#'
#' @return A `data.frame` of model contents for all models in the passed project
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_id ID of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_models <- models(conn, 4L)
#' }
models <- function(zoltar_connection, project_id) {
  the_project_info <- project_info(zoltar_connection, project_id)
  id_column <- c()
  url_column <- c()
  project_id_column <- c()
  owner_id_column <- c()
  name_column <- c()
  description_column <- c()
  home_url_column <- c()
  aux_data_url_column <- c()  # might be NULL. substitute NA if so
  for (model_url in the_project_info$models) {
    model_json <- get_resource(zoltar_connection, model_url)
    id_column <- append(id_column, model_json$id)
    url_column <- append(url_column, model_url)
    project_id_column <- append(project_id_column, id_for_url(model_json$project))

    # owner_id_column <- append(owner_id_column, id_for_url(model_json$owner))
    owner_id_value <- if (is.null(model_json$owner)) NA else id_for_url(model_json$owner)
    owner_id_column <- append(owner_id_column, owner_id_value)

    name_column <- append(name_column, model_json$name)
    description_column <- append(description_column, model_json$description)
    home_url_column <- append(home_url_column, model_json$home_url)

    aux_data_value <- if (is.null(model_json$aux_data_url)) NA else model_json$aux_data_url
    aux_data_url_column <- append(aux_data_url_column, aux_data_value)
  }
  data.frame(id=id_column, url=url_column, project_id=project_id_column, owner_id=owner_id_column, name=name_column,
    description=description_column, home_url=home_url_column, aux_data_url=aux_data_url_column,
    stringsAsFactors=FALSE)
}


#
# ---- model functions ----
#

YYYYMMDD_format <- "%Y%m%d"  # for as.Date()


#' Get information about a model
#'
#' @return A `list` of model information for the passed model_id
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param model_id ID of a model in zoltar_connection's models
#' @export
#' @examples \dontrun{
#'   the_model_info <- model_info(conn, 26L)
#' }
model_info <- function(zoltar_connection, model_id) {
  get_resource(zoltar_connection, url_for_model_id(zoltar_connection, model_id))
}


#' Get a model's forecasts
#'
#' @return A `data.frame` of forecast information for the passed model
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param model_id ID of a model in zoltar_connection's models
#' @export
#' @examples \dontrun{
#'   the_forecasts <- forecasts(conn, 26L)
#' }
forecasts <- function(zoltar_connection, model_id) {
  model_json <- get_resource(zoltar_connection, url_for_model_id(zoltar_connection, model_id))
  id_column <- c()
  url_column <- c()                # character
  timezero_date_column <- c()      # Date
  data_version_date_column <- c()  # Date - NA for NULL data_version_dates
  for (forecast_idx in seq_along(model_json$forecasts)) {
    forecast_json <- model_json$forecasts[[forecast_idx]]
    if (is.null(forecast_json$forecast)) { next }

    # append to the columns. regarding NULL values: we know forecast_json$forecast (url) and
    # forecast_json$timezero_date are non-NULL, but forecast_json$data_version_date might be NULL. if so, we use NA
    # for it b/c dealing with NULLs is a confusing pain. for both forecast_json$timezero_date and
    # forecast_json$data_version_date, we convert to Date objects
    id_column <- append(id_column, id_for_url(forecast_json$forecast))
    url_column <- append(url_column, forecast_json$forecast)
    timezero_date_column <- append(timezero_date_column, as.Date(forecast_json$timezero_date, format=YYYYMMDD_format))

    dvd_value <- if (is.null(forecast_json$data_version_date)) NA else forecast_json$data_version_date
    data_version_date_column <- append(data_version_date_column, as.Date(dvd_value, format=YYYYMMDD_format))
  }
  data.frame(id=id_column, url=url_column, timezero_date=timezero_date_column,
    data_version_date=data_version_date_column, stringsAsFactors=FALSE)
}


#' Upload a forecast
#'
#' This function submits forecast data to the server for uploading. Returns an UploadFileJob object that can be used to
# 'track the upload's progress. (Uploads are processed in a queue, which means they are delayed until their turn comes
#' up, which depends on the number of current uploads in the queue. Zoltar tracks these via `UploadFileJob` objects.)
#'
#' @return An UploadFileJob id for the upload
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param model_id ID of a model in zoltar_connection's projects
#' @param timezero_date The date of the project timezero you are uploading for. it is a string in format YYYYMMDD
#' @param forecast_data Forecast data as a `list` in the Zoltar standard format
#' @export
#' @examples \dontrun{
#'   upload_file_job_id <- upload_forecast(conn, 26L, "20170117", "tests/testthat/EW1-KoTsarima-2017-01-17-small.json")
#' }
upload_forecast <- function(zoltar_connection, model_id, timezero_date, forecast_data) {
  if (!(inherits(forecast_data, "list"))) {
    stop("forecast_data was not a `list`", call. = FALSE)
  }

  forecasts_url <- url_for_model_forecasts_id(zoltar_connection, model_id)
  re_authenticate_if_necessary(zoltar_connection)
  message(paste0("upload_forecast(): POST: ", forecasts_url))
  temp_json_file <- tempfile(pattern = "forecast", fileext = ".json")
  response <- httr::POST(
    url=forecasts_url,
    add_auth_headers(zoltar_connection),
    body=list(data_file=httr::upload_file(temp_json_file), timezero_date=timezero_date))
  # the Zoltar API returns 400 if there was an error POSTing. the content is JSON with a $error key that contains the
  # error message
  if (response$status_code == 400) {
    json_response <- httr::content(response, "parsed")
    stop(json_response$error, call. = FALSE)
  }

  upload_file_job_json <- httr::content(response, "parsed")
  upload_file_job_json$id  # throw away rest of json and let upload_file_job_info() reload/refresh it
}


#' Delete a forecast
#'
#' Deletes the forecast with the passed ID. This is permanent and cannot be undone.
#'
#' @return None
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param forecast_id ID of a forecast in zoltar_connection's forecasts
#' @export
#' @examples \dontrun{
#'   delete_forecast(conn, 46L)
#' }
delete_forecast <- function(zoltar_connection, forecast_id) {
  delete_resource(zoltar_connection, url_for_forecast_id(zoltar_connection, forecast_id))
}


#
# ---- forecast functions ----
#

#' Gets a forecast's information
#'
#' @return A `list` of forecast information for the passed forecast_id
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param forecast_id ID of a forecast in zoltar_connection's forecasts
#' @export
#' @examples \dontrun{
#'   the_forecast_info <- forecast_info(conn, 46L)
#' }
forecast_info <- function(zoltar_connection, forecast_id) {
  get_resource(zoltar_connection, url_for_forecast_id(zoltar_connection, forecast_id))
}


#' Gets a forecast's data
#'
#' @return Forecast data as a `list` in the Zoltar standard format - see \url{https://www.zoltardata.com/docs#forecasts}
#'   An example: tests/testthat/EW1-KoTsarima-2017-01-17-small.json
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param forecast_id ID of a forecast in zoltar_connection's forecasts
#' @export
#' @examples \dontrun{
#'   forecast_data <- download_forecast(conn, 46L)
#' }
download_forecast <- function(zoltar_connection, forecast_id) {
  forecast_data_url <- url_for_forecast_data_id(zoltar_connection, forecast_id)
  get_resource(zoltar_connection, forecast_data_url)
}


TARGET_TO_UNIT <- list(
  "Season peak percentage"="percent",
  "1 wk ahead"="percent",
  "2 wk ahead"="percent",
  "3 wk ahead"="percent",
  "4 wk ahead"="percent",
  "Season onset"="week",
  "Season peak week"="week")

# todo xx these are project-specific to the CDC ensemble
BINCAT_TARGET_NAMES <- c("Season onset", "Season peak week")
BINLWR_TARGET_NAMES <- c("Season peak percentage", "1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
CDC_POINT_ROW_TYPE <- "Point"
CDC_BIN_ROW_TYPE <- "Bin"


#' Converts forecast data from Zoltar's native `list` format to a CDC-specific `data.frame`
#'
#' @return Forecast data as a CDC format `data.frame`. Only the "predictions" section is included (the "meta" is
#'   excluded).
#' @param forecast_data A forecast's data in Zoltar's native `list` such as returned by \code{\link{download_forecast}}
#' @export
#' @examples \dontrun{
#'   forecast_data <- download_forecast(conn, 46L)
#'   forecast_data_frame <- cdc_data_frame_from_forecast_data(forecast_data)
#' }
cdc_data_frame_from_forecast_data <- function(forecast_data) {
  if (is.null(forecast_data$predictions)) {
    stop("no $predictions found in forecast_data", call. = FALSE)
  }

  location_column <- c()  # chr b/c all locations are strings
  target_column <- c()  # ""
  type_column <- c()  # ""
  unit_column <- c()  # ""
  bin_start_incl_column <- c()  # also chr, but b/c values can be 'none', which convert the vector to chr
  bin_end_notincl_column <- c()  # ""
  value_column <- c()  # ""
  for (prediction_idx in seq_along(forecast_data$predictions)) {
    prediction_json <- forecast_data$predictions[[prediction_idx]]
    prediction_class <- prediction_json$class
    if (!(prediction_class %in% c("BinCat", "BinLwr", "Point"))) {
      stop(paste0("invalid prediction_json class: '", prediction_class, "'"), call. = FALSE)
    }

    target <- prediction_json$target
    if (!(target %in% names(TARGET_TO_UNIT))) {
      stop(paste0("invalid prediction_json target: '", target, "'"), call. = FALSE)
    }

    location <- prediction_json$location
    row_type <- if (prediction_class == "Point") CDC_POINT_ROW_TYPE else CDC_BIN_ROW_TYPE
    unit <- TARGET_TO_UNIT[[target]]
    prediction <- prediction_json$prediction
    if (row_type == CDC_POINT_ROW_TYPE) {  # "Point". output a single point row
      location_column <- append(location_column, location)
      target_column <- append(target_column, target)
      type_column <- append(type_column, row_type)
      unit_column <- append(unit_column, unit)
      bin_start_incl_column <- append(bin_start_incl_column, NA)
      bin_end_notincl_column <- append(bin_end_notincl_column, NA)
      value_column <- append(value_column, prediction$value)  # m/be "none"
    } else if (prediction_class == "BinCat") {  # "BinCat". output multiple bin rows
      for (cat_prob_idx in seq_along(prediction$cat)) {
        cat <- prediction$cat[[cat_prob_idx]]  # m/be "none"
        prob <- prediction$prob[[cat_prob_idx]]
        location_column <- append(location_column, location)
        target_column <- append(target_column, target)
        type_column <- append(type_column, row_type)
        unit_column <- append(unit_column, unit)
        bin_start_incl_column <- append(bin_start_incl_column, cat)
        bin_end_notincl_column <- append(bin_end_notincl_column, recode_cat_to_bin_end_notincl(cat))
        value_column <- append(value_column, prob)
      }
    } else {  # "BinLwr". output multiple bin rows
      for (lwr_prob_idx in seq_along(prediction$lwr)) {
        lwr <- prediction$lwr[[lwr_prob_idx]]
        prob <- prediction$prob[[lwr_prob_idx]]
        location_column <- append(location_column, location)
        target_column <- append(target_column, target)
        type_column <- append(type_column, row_type)
        unit_column <- append(unit_column, unit)
        bin_start_incl_column <- append(bin_start_incl_column, lwr)
        bin_end_notincl_column <- append(bin_end_notincl_column, if (lwr == 13) 100 else lwr + 0.1)
        value_column <- append(value_column, prob)
      }
    }
  }
  data.frame(location=location_column, target=target_column, type=type_column, unit=unit_column,
    bin_start_incl=bin_start_incl_column, bin_end_notincl=bin_end_notincl_column, value=value_column,
    stringsAsFactors=FALSE)
}


recode_cat_to_bin_end_notincl <- function(cat) {
  cat_to_new_cat <- list(
    "40" = "41",
    "41" = "42",
    "42" = "43",
    "43" = "44",
    "44" = "45",
    "45" = "46",
    "46" = "47",
    "47" = "48",
    "48" = "49",
    "49" = "50",
    "50" = "51",
    "51" = "52",
    "52" = "53",
    "1" = "2",
    "2" = "3",
    "3" = "4",
    "4" = "5",
    "5" = "6",
    "6" = "7",
    "7" = "8",
    "8" = "9",
    "9" = "10",
    "10" = "11",
    "11" = "12",
    "12" = "13",
    "13" = "14",
    "14" = "15",
    "15" = "16",
    "16" = "17",
    "17" = "18",
    "18" = "19",
    "19" = "20",
    "20" = "21",
    "none" = "none")
  cat_to_new_cat[[cat]]
}


#' Loads and converts a CDC CSV file to Zoltar's native `list` format
#'
#' @return cdc_csv_file's data as Zoltar's native `list` format, but only the "predictions" item, and not "meta"
#' @param cdc_csv_file A CDC CSV file. todo xx note that we currently do minimal validation of the data :-/
#' @export
#' @examples \dontrun{
#'   cdc_csv_file <- "my_forecast.cdc.csv"
#'   forecast_data <- forecast_data_from_cdc_csv_file(cdc_csv_file)
#' }
forecast_data_from_cdc_csv_file <- function(cdc_csv_file) {
  cdc_data_frame <- read.csv(cdc_csv_file, stringsAsFactors=FALSE)  # "NA" -> NA
  forecast_data_from_cdc_data_frame(cdc_data_frame)
}


forecast_data_from_cdc_data_frame <- function (cdc_data_frame) {  # testable internal function that does the work
  # print(c('yy1', dim(cdc_data_frame), names(cdc_data_frame)))
  names(cdc_data_frame) <- sapply(names(cdc_data_frame), tolower)


  # validate cdc_data_frame
  if (!(inherits(cdc_data_frame, "data.frame"))) {
    stop("cdc_data_frame was not a `data.frame`", call. = FALSE)
  }

  if ((length(cdc_data_frame) == 0) || (names(cdc_data_frame) !=
      c("location", "target", "type", "unit", "bin_start_incl", "bin_end_notincl", "value"))) {
    stop("cdc_data_frame did not have required columns", call. = FALSE)
  }

  predictions <- list()
  cdc_data_frame_grouped <- cdc_data_frame %>% dplyr::group_by(location, target, type) %>% group_data()
  for(group_idx in seq_len(nrow(cdc_data_frame_grouped))) {
    group_row <- cdc_data_frame_grouped[group_idx,]  # group_row$location,  group_row$target,  group_row$type
    point_values <- list()  # NB: should only be one point row, but collect all (but don't validate here)
    bincat_cats <- list() ; bincat_probs <- list()
    binlwr_lwrs  <- list() ; binlwr_probs <- list()
    for (group_rows_idx in seq_along(group_row$.rows[[1]])) {
      cdc_data_frame_idx <- group_row$.rows[[1]][group_rows_idx]
      # NB: cdc_row values could come in as numbers or strings, depending on the source csv file values
      cdc_row <- cdc_data_frame[cdc_data_frame_idx,]  # cdc_row$bin_start_incl, cdc_row$bin_end_notincl, cdc_row$value
      if (group_row$type == CDC_POINT_ROW_TYPE) {
        point_value <- if (group_row$target %in% BINCAT_TARGET_NAMES) as.character(cdc_row$value) else as.numeric(cdc_row$value)
        point_values <- append(point_values, point_value)
      } else if (group_row$target %in% BINCAT_TARGET_NAMES) {
        bincat_cats <- append(bincat_cats, as.character(cdc_row$bin_start_incl))
        bincat_probs <- append(bincat_probs, as.numeric(cdc_row$value))
      } else if (group_row$target %in% BINLWR_TARGET_NAMES) {
        binlwr_lwrs <- append(binlwr_lwrs, as.numeric(cdc_row$bin_start_incl))
        binlwr_probs <- append(binlwr_probs, as.numeric(cdc_row$value))
      } else {
        stop("unexpected bin target_name")  # todo more details
      }
    }

    # add the actual prediction dicts
    if (length(bincat_cats) > 0) {  # yes warning: "NAs introduced by coercion"
      prediction <- list("location"=group_row$location, "target"=group_row$target, "class"="BinCat",
                         "prediction"=list("cat"=bincat_cats, "prob"=bincat_probs))
      predictions[[length(predictions) + 1]] <- prediction
    }
    if (length(binlwr_lwrs) > 0) {  # no warning
      prediction <- list("location"=group_row$location, "target"=group_row$target, "class"="BinLwr",
                         "prediction"=list("lwr"=binlwr_lwrs, "prob"=binlwr_probs))
      predictions[[length(predictions) + 1]] <- prediction
    }
    if (length(point_values) > 0) {  # yes warning
      for (point_value in point_values) {
        prediction <- list("location"=group_row$location, "target"=group_row$target, "class"="Point",
                           "prediction"=list("value"=point_value))
        predictions[[length(predictions) + 1]] <- prediction
      }
    }

  }

  list("predictions"=predictions)
}


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
#' @return A `list` of upload information for the passed upload_file_job_id. it has these names:
#'   id, url, status, user, created_at, updated_at, failure_message, filename, input_json, output_json
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param upload_file_job_id ID of a job in zoltar_connection that was uploaded via \code{\link{upload_forecast}}
#' @export
#' @examples \dontrun{
#'   the_upload_info <- upload_info(conn, 287L)
#' }
upload_info <- function(zoltar_connection, upload_file_job_id) {
  ufj_json <- get_resource(zoltar_connection, url_for_upload_file_job_id(zoltar_connection, upload_file_job_id))
  ufj_json$status <- status_as_str(ufj_json$status)
  ufj_json$created_at <- as.Date(ufj_json$created_at)
  ufj_json$updated_at <- as.Date(ufj_json$updated_at)
  ufj_json
}


#
# ZoltarSession class. used internally only
#

new_session <- function(zoltar_connection) {
  self <- structure(environment(), class="ZoltarSession")
  zoltar_connection <- zoltar_connection
  token <- get_token(self)  # expects zoltar_connection
  self
}


# returns the JWT token string obtained from zoltar. it has decoded contents that look like this:
# - header:  {"typ": "JWT", "alg": "HS256"}
# - payload: {"user_id": 3, "username": "model_owner1", "exp": 1558442805, "email": ""}
get_token <- function(zoltar_session) {
  zoltar_connection <- zoltar_session$zoltar_connection
  token_auth_url <- url_for_token_auth(zoltar_connection)
  message(paste0("get_token(): POST: ", token_auth_url))
  response <-
    httr::POST(
      url=token_auth_url,
      httr::accept_json(),
      body=list(
        username=zoltar_connection$username,
        password=zoltar_connection$password
      )
    )
  httr::stop_for_status(response)
  json_content <- httr::content(response, "parsed")
  json_content$token
}


# returns a POSIXct for the zoltar_session's token. see notes in is_token_expired() for details on extracting the date
token_expiration_date <- function(zoltar_session) {
  token_split <- strsplit(zoltar_session$token, ".", fixed=TRUE)  # 3 parts: header, payload, and signature
  payload_encoded <- token_split[[1]][2]
  payload_decoded <- base64url::base64_urldecode(payload_encoded)
  payload <- jsonlite::fromJSON(payload_decoded)
  exp_timestamp_utc <- payload$exp
  exp_timestamp_date <- .POSIXct(exp_timestamp_utc, tz="UTC")
  exp_timestamp_date
}


# returns TRUE if zoltar_session's token is expired, and FALSE if still valid. details: based on how Zoltar implements
# JWT, we determine expiration by comparing the current datetime to the token's payload's "exp" field. its value is a
# POSIX timestamp of a UTC date and time as returned by datetime.utcnow().timestamp() - https://docs.python.org/3.6/library/datetime.html#datetime.datetime.utcnow . xx
is_token_expired <- function(zoltar_session) {
  token_expiration_date(zoltar_session) <= Sys.time()  # now
}
