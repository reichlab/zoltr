library(httr)
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


stopifnot_authenticated <- function(zoltar_connection) {
  stopifnot(inherits(zoltar_connection, "ZoltarConnection"))
  stopifnot(inherits(zoltar_connection$session, "ZoltarSession"))
}


# returns an API URL for the passed project_id, sans trailing slash
url_for_project_id <- function(zoltar_connection, project_id) {
  stopifnot_authenticated(zoltar_connection)
  paste0(zoltar_connection$host, '/api/project/', project_id)
}


url_for_model_id <- function(zoltar_connection, model_id) {
  stopifnot_authenticated(zoltar_connection)
  paste0(zoltar_connection$host, '/api/model/', model_id)
}


url_for_model_forecasts_id <- function(zoltar_connection, model_id) {
  paste0(url_for_model_id(zoltar_connection, model_id), '/forecasts/')
}


url_for_upload_file_job_id <- function(zoltar_connection, upload_file_job_id) {
  stopifnot_authenticated(zoltar_connection)
  paste0(zoltar_connection$host, '/api/uploadfilejob/', upload_file_job_id)
}


url_for_forecast_id <- function(zoltar_connection, forecast_id) {
  stopifnot_authenticated(zoltar_connection)
  paste0(zoltar_connection$host, '/api/forecast/', forecast_id)
}


url_for_forecast_data_id <- function(zoltar_connection, forecast_id) {
  paste0(url_for_forecast_id(zoltar_connection, forecast_id), '/data/')
}


url_for_token_auth <- function(zoltar_connection) {
  paste0(zoltar_connection$host, '/api-token-auth/')
}


add_auth_headers <- function(zoltar_connection) {
  stopifnot_authenticated(zoltar_connection)
    httr::add_headers("Authorization"=paste0("JWT ", zoltar_connection$session$token))
}


# deletes the resource at the passed URL
delete_resource <- function(zoltar_connection, url) {
  stopifnot_authenticated(zoltar_connection)
  response <- httr::DELETE(url=url, httr::accept_json(), add_auth_headers(zoltar_connection))
  httr::stop_for_status(response)
}


#
# ---- ZoltarConnection class ----
#

#' create a new connection to a Zoltar host
#'
#' Returns a new connection object, which is the starting point for working with the Zoltar API. Once you have the
#' connection you can call \code{\link{z_authenticate}} on it, and then call \code{\link{projects}} to get a list of
#' Project objects to start working with.
#'
#' @return a ZoltarConnection object
#' @param host The Zoltar site to connect to. Defaults to \url{https://zoltardata.com}
#' @export
new_connection <- function(host="zoltardata.com") {
  self <- structure(environment(), class="ZoltarConnection")  # todo xx change to a list?
  host <- host
  username <- NULL
  password <- NULL
  session <- NULL
  self
}


#' @export
print.ZoltarConnection <-
  function(x, ...) {
    cat(class(x), " '", x$host, "' ", if (is.null(x$session)) "(no session)" else "(authenticated)", "\n", sep='')
  }


#' log in to a Zoltar host
#'
#' Returns a new ZoltarConnection object, which is the starting point for working with the Zoltar API.
#' Once you have the connection you can call z_authenticate() on it, and call projects() to get a list of objects to
#' start working with.
#'
#' @return none
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param username username for the account to use on the connection's host
#' @param password password ""
#' @export
z_authenticate <-
  function(zoltar_connection,
           username,
           password) {
    UseMethod("z_authenticate")
  }

#' @export
z_authenticate.default <-
  function(zoltar_connection,
           username,
           password) {
    zoltar_connection$username <- username
    zoltar_connection$password <- password
    zoltar_connection$session <- new_session(zoltar_connection)
  }


json_for_url <- function(zoltar_connection, url, ...) {  # private
  UseMethod("json_for_url")
}

json_for_url.default <- function(zoltar_connection, url, ...) {
  stopifnot(inherits(zoltar_connection$session, "ZoltarSession"))
  response <- httr::GET(url=url, httr::accept_json(), add_auth_headers(zoltar_connection))
  httr::stop_for_status(response)
  json_content <- httr::content(response, "parsed")
  json_content
}


#' get a data.frame of all projects' metadata
#'
#' Returns a data.frame of all projects' contents.
#'
#' @return a data.frame of project contents
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @export
projects <- function(zoltar_connection) {
  UseMethod("projects")
}

#' @export
projects.default <- function(zoltar_connection) {
  projects_json <- json_for_url(zoltar_connection, paste0(zoltar_connection$host, '/api/projects/'))
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
    owner_id_column <- append(owner_id_column, id_for_url(project_json$owner))
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

#' get a project's information
#'
#' @return a list of project information for the passed project_id
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param project_id id of a project in zoltar_connection's projects
#' @export
project_info <- function(zoltar_connection, project_id) {
  json_for_url(zoltar_connection, url_for_project_id(zoltar_connection, project_id))
}


#' get a project's scores
#'
#' @return a data.frame of score data for all models in the passed Project object
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param project_id id of a project in zoltar_connection's projects
#' @export
scores <- function(zoltar_connection, project_id) {
  scores_url <- paste0(url_for_project_id(zoltar_connection, project_id), '/score_data/')
  response <- httr::GET(url=scores_url, add_auth_headers(zoltar_connection))
  httr::stop_for_status(response)
  httr::content(response, encoding="UTF-8")
}


#' get a project's models
#'
#' @return a data.frame of model contents for all models in the passed project
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param project_id id of a project in zoltar_connection's projects
#' @export
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
    model_json <- json_for_url(zoltar_connection, model_url)
    id_column <- append(id_column, model_json$id)
    url_column <- append(url_column, model_url)
    project_id_column <- append(project_id_column, id_for_url(model_json$project))
    owner_id_column <- append(owner_id_column, id_for_url(model_json$owner))
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


#' get a model's information
#'
#' @return a list of model information for the passed model_id
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param model_id id of a model in zoltar_connection's models
#' @export
model_info <- function(zoltar_connection, model_id) {
  json_for_url(zoltar_connection, url_for_model_id(zoltar_connection, model_id))
}


#' get a model's forecasts
#'
#' @return a data.frame of forecast information for the passed model
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param model_id id of a model in zoltar_connection's models
#' @export
forecasts <- function(zoltar_connection, model_id) {
  model_json <- json_for_url(zoltar_connection, url_for_model_id(zoltar_connection, model_id))
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


# upload_forecast() helper that enables easier testing
post_forecast <- function(zoltar_connection, model_id, timezero_date, forecast_csv_file) {
  response <- httr::POST(
    url=url_for_model_forecasts_id(zoltar_connection, model_id),
    add_auth_headers(zoltar_connection),
    body=list(data_file=httr::upload_file(forecast_csv_file), timezero_date=timezero_date))
  # the Zoltar API returns 400 if there was an error POSTing. the content is JSON with a $error key that contains the
  # error message
  if (response$status_code == 400) {
    json_response <- httr::content(response, "parsed")
    stop(json_response$error, call. = FALSE)
  } else {
    httr::content(response, "parsed")
  }
}


#' upload a forecast
#'
#' Function submits a forecast file to the server for uploading. returns an UploadFileJob object that can
#' be used to track the upload's progress. (Uploads are processed in a queue, which means they are delayed until their
#' turn comes up, which depends on the number of current uploads in the queue.)
#'
#' @return UploadFileJob id for the upload
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param model_id id of a model in zoltar_connection's projects
#' @param timezero_date the date of the project timezero you are uploading for. it is a string in format YYYYMMDD
#' @param forecast_csv_file a CSV file in the Zoltar standard format - see \url{https://www.zoltardata.com/docs#forecasts}
#' @export
upload_forecast <- function(zoltar_connection, model_id, timezero_date, forecast_csv_file) {
  upload_file_job_json <- post_forecast(zoltar_connection, model_id, timezero_date, forecast_csv_file)
  upload_file_job_json$id  # throw away rest of json and let upload_file_job_info() reload/refresh it
}


#' delete a forecast
#'
#' Deletes the forecast with the passed ID. This is permanent and cannot be undone.
#'
#' @return none
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param forecast_id id of a forecast in zoltar_connection's forecasts
#' @export
delete_forecast <- function(zoltar_connection, forecast_id) {
  delete_resource(zoltar_connection, url_for_forecast_id(zoltar_connection, forecast_id))
}


#
# ---- forecast functions ----
#

#' get a forecast's information
#'
#' @return a list of forecast information for the passed forecast_id
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param forecast_id id of a forecast in zoltar_connection's forecasts
#' @export
forecast_info <- function(zoltar_connection, forecast_id) {
  json_for_url(zoltar_connection, url_for_forecast_id(zoltar_connection, forecast_id))
}


#' get a forecast's data
#'
#' @return forecast data in the requested format
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param forecast_id id of a forecast in zoltar_connection's forecasts
#' @param is_json a boolean specifying whether the forecast is in JSON or CSV format
#' @export
forecast_data <- function(zoltar_connection, forecast_id, is_json) {
  forecast_data_url <- url_for_forecast_data_id(zoltar_connection, forecast_id)
  if (is_json) {
    json_for_url(zoltar_connection, forecast_data_url)
  } else {  # CSV
    # todo fix api_views.forecast_data() to use proper accept type rather than 'format' query parameter
    response <- httr::GET(url=forecast_data_url, add_auth_headers(zoltar_connection), query=list(format="csv"))
    httr::stop_for_status(response)
    httr::content(response, encoding="UTF-8")
  }
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


#' get an upload's information
#'
#' Gets an upload's information that can#' be used to track the upload's progress. (Uploads are processed in a queue,
#  which means they are delayed until their turn comes up, which depends on the number of current uploads in the queue.)
#'
#' @return a list of upload information for the passed upload_file_job_id. it has these names:
#'   id, url, status, user, created_at, updated_at, failure_message, filename, input_json, output_json
#' @param zoltar_connection a ZoltarConnection object as returned by \code{\link{new_connection}}
#' @param upload_file_job_id id of a job in zoltar_connection that was uploaded via \code{\link{upload_forecast}}
#' @export
upload_info <- function(zoltar_connection, upload_file_job_id) {
  ufj_json <- json_for_url(zoltar_connection, url_for_upload_file_job_id(zoltar_connection, upload_file_job_id))
  ufj_json$status <- status_as_str(ufj_json$status)
  ufj_json$created_at <- as.Date(ufj_json$created_at)
  ufj_json$updated_at <- as.Date(ufj_json$updated_at)
  ufj_json
}


#
# ZoltarSession class. used internally only
#

new_session <- function(zoltar_connection) {
  self <- structure(environment(), class="ZoltarSession")  # todo xx change to a list?
  zoltar_connection <- zoltar_connection
  token <- get_token(self)  # expects zoltar_connection
  self
}


get_token <- function(zoltar_session, ...) {
  UseMethod("get_token")
}

get_token.default <- function(zoltar_session, ...) {
  response <-
    httr::POST(
      url=url_for_token_auth(zoltar_session$zoltar_connection),
      httr::accept_json(),
      body=list(
        username=zoltar_session$zoltar_connection$username,
        password=zoltar_session$zoltar_connection$password
      )
    )
  httr::stop_for_status(response)
  json_content <- httr::content(response, "parsed")
  json_content$token
}

