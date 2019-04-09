library(httr)
library(readr)  # apparently required by httr


#
# ZoltarConnection class
#

#' create a new connection to a Zoltar host
#'
#' Returns a new connection object, which is the starting point for working with the Zoltar API. Once you have the
#' connection you can call \code{\link{z_authenticate}} on it, and then call \code{\link{projects}} to get a list of
#' \code{\link{Project}} objects to start working with.
#'
#' @return a \code{\link{ZoltarConnection}} object
#' @param host The Zoltar site to connect to. Defaults to \url{https://zoltardata.com}
#' @export
new_connection <- function(host = "zoltardata.com") {
  self <- structure(environment(), class = "ZoltarConnection")
  host <- host
  username <- NULL
  password <- NULL
  session <- NULL
  self
}


#' @export
print.ZoltarConnection <- function(zoltar_connection, ...) {
  cat(class(zoltar_connection), " ",
  zoltar_connection$host,
  ". username, password=", if (is.null(zoltar_connection$username)) "<no username>" else zoltar_connection$username,
  ", ", if (is.null(zoltar_connection$password)) "<no password>" else zoltar_connection$password,
  ". session=", if (is.null(zoltar_connection$session)) "<no session>" else zoltar_connection$session$token,
  "\n", sep='')
}


#' log in to a Zoltar host
#'
#' Returns a new \code{\link{ZoltarConnection}} object, which is the starting point for working with the Zoltar API.
#' Once you have the connection you can call z_authenticate() on it, and call projects() to get a list of objects to
#' start working with.
#'
#' @return none
#' @param zoltar_connection a \code{\link{ZoltarConnection}} object as returned by \code{\link{new_connection}}
#' @param username username for the account to use on the connection's host
#' @param password password ""
#' @export
z_authenticate <- function(zoltar_connection, username, password, ...) {
  UseMethod("z_authenticate")
}

#' @export
z_authenticate.default <- function(zoltar_connection, username, password, ...) {
  zoltar_connection$username <- username
  zoltar_connection$password <- password
  zoltar_connection$session <- new_session(zoltar_connection)
}


json_for_url <- function(zoltar_connection, url, ...) {  # private
  UseMethod("json_for_url")
}

json_for_url.default <- function(zoltar_connection, url, ...) {
  stopifnot(is(zoltar_connection$session, "ZoltarSession"))
  response <- httr::GET(url = url,
                        accept_json(),
                        add_headers("Authorization" = paste0("JWT ", zoltar_connection$session$token)))
  stop_for_status(response)
  json_content <- content(response, "parsed")
  json_content
}


#' get a list of all projects
#'
#' Returns a list of \code{\link{Project}} objects, which are the starting point for working with the API.
#'
#' @return a list of \code{\link{Project}} objects
#' @param zoltar_connection a \code{ZoltarConnection} object as returned by \code{\link{new_connection}}
#' @export
projects <- function(zoltar_connection, ...) {
  UseMethod("projects")
}

#' @export
projects.default <- function(zoltar_connection, ...) {
  projects_json <- json_for_url(zoltar_connection, paste0(zoltar_connection$host, '/api/projects/'))
  projects <- lapply(projects_json,
    function(project_json) {
      new_project(zoltar_connection, project_json$url)
    })
  projects
}


#
# ZoltarSession class. used internally only
#

new_session <- function(zoltar_connection) {  # private
  self <- structure(environment(), class = "ZoltarSession")
  zoltar_connection <- zoltar_connection
  token <- get_token(self)  # expects zoltar_connection
  self
}


get_token <- function(zoltar_session, ...) {
  UseMethod("get_token")
}

get_token.default <- function(zoltar_session, ...) {
  response <- httr::POST(url = paste0(zoltar_session$zoltar_connection$host, '/api-token-auth/'),
                         accept_json(),
                         body = list(username = zoltar_session$zoltar_connection$username,
                                     password = zoltar_session$zoltar_connection$password))
  stop_for_status(response)
  json_content <- content(response, "parsed")
  token <- json_content$token
  token
}


#
# ZoltarResource abstract class. used internally only
#

new_resource <- function(zoltar_connection, url) {
  self <- structure(environment(), class = "ZoltarResource")
  zoltar_connection <- zoltar_connection
  url <- url
  json <- NULL
  refresh(self)  # update json
  self
}


print.ZoltarResource <- function(zoltar_resource, ...) {
  cat(class(zoltar_resource)[1], " ",
  zoltar_resource$url, " ",
  if (is.null(zoltar_resource$json)) "<no JSON>" else paste0("len=", length(zoltar_resource$json)),
  "\n", sep='')
}


#' update a resource
#'
#' Updates the internal (JSON) data associated with a \code{\link{Project}}, \code{\link{Model}}, \code{\link{Forecast}},
#' or \code{\link{UploadFileJob}} object. Refreshing is required when you've changed something on the server that would
#' change what the API would return after the change. For example, creating a new Project on the site means the list
#' that \code{\link{projects}} returns would be stale and need updating. You'll also need to refresh after changing an
#' object's contents on the site, such as a model name or its forecasts. The API works this way, instead of
#' auto-refresh, because the latter would require querying the server every time.
#'
#' @return none
#' @param zoltar_resource the object to refresh
#' @export
refresh <- function(zoltar_resource, ...) {
  UseMethod("refresh")
}

#' @export
refresh.default <- function(zoltar_resource, ...) {
  zoltar_resource$json <- json_for_url(zoltar_resource$zoltar_connection, zoltar_resource$url)
}


#' delete a resource
#'
#' Deletes the passed \code{\link{Project}}, \code{\link{Model}}, or \code{\link{Forecast}} object. This is permanent
#' and cannot be undone. Also, it will \emph{cascade} the delete to any related data in the database, such as what would
#' happen when you delete a model, which would delete all of its forecasts and their data.
#'
#' @return none
#' @param zoltar_resource the object to refresh
#' @export
delete <- function(zoltar_resource, ...) {
  UseMethod("delete")
}

#' @export
delete.default <- function(zoltar_resource, ...) {
  response <- httr::DELETE(url = zoltar_resource$url,
                           accept_json(),
                           add_headers("Authorization" = paste0("JWT ", zoltar_resource$zoltar_connection$session$token)))
  stop_for_status(response)
}


#' get a resource's id
#'
#' A \emph{getter} function that returns the id of the passed \code{\link{Project}}, \code{\link{Model}}, or
# \code{\link{Forecast}} object.
#'
#' @return none
#' @param zoltar_resource the object to get the id of
#' @export
id <- function(zoltar_resource, ...) {
  UseMethod("id")
}

#' @export
id.default <- function(zoltar_resource, ...) {
  zoltar_resource$json$id
}


#
# Project class - ZoltarResource subclass
#

#' make a new project (lower level function)
#'
#' Constructor that returns a new \code{\link{Project}} object for a particular project's url. Not normally called
#' directly - most users use the \code{\link{projects}} function.
#'
#' @return a \code{\link{Project}} object
#' @param zoltar_connection a \code{ZoltarConnection} object as returned by \code{\link{new_connection}}
#' @param url host address of the Project, e.g., \emph{http://example.com/api/project/1/}.
#' @export
new_project <- function(zoltar_connection, url) {
  self <- new_resource(zoltar_connection, url)
  class(self) <- append("Project", class(self))
  self
}


#' get a project's models
#'
#' Returns a list of \code{\link{Model}} objects in the passed project.
#'
#' @return a list of \code{\link{Model}} objects
#' @param project a \code{\link{Project}} object
#' @export
models <- function(project, ...) {
  UseMethod("models")
}

#' @export
models.default <- function(project, ...) {
  models_urls <- project$json$models
  models <- lapply(models_urls,
    function(model_url) {
      new_model(project$zoltar_connection, model_url)
    })
  models
}


#' get a resource's name
#'
#' A \emph{getter} function that returns the name of the passed \code{\link{Project}}.
#'
#' @return none
#' @param zoltar_resource the object to get the id of
#' @export
name <- function(project, ...) {
  UseMethod("name")
}

#' @export
name.default <- function(project, ...) {
  project$json$name
}


#' get a project's scores
#'
#' @return score data associated for all models in the passed \code{\link{Project}} object in CSV format
#' @export
scores <- function(project, ...) {
  UseMethod("scores")
}

#' @export
scores.default <- function(project, ...) {
  scores_url <- paste0(project$url, 'score_data/')
  response <- httr::GET(url = scores_url,
                        add_headers("Authorization" = paste0("JWT ", project$zoltar_connection$session$token)))
  stop_for_status(response)
  content(response, encoding="UTF-8")
}


#
# Model class - ZoltarResource subclass
#

#' make a new model (lower level function)
#'
#' Constructor that returns a new \code{\link{Model}} object for a particular model's url. Not normally called
#' directly.
#'
#' @return a \code{\link{Model}} object
#' @param zoltar_connection a \code{ZoltarConnection} object as returned by \code{\link{new_connection}}
#' @param url host address of the Model, e.g., \emph{http://example.com/api/model/1/}.
#' @export
new_model <- function(zoltar_connection, url) {
  self <- new_resource(zoltar_connection, url)
  class(self) <- append("Model", class(self))
  self
}


#' get a model's forecasts
#'
#' @return a list of \code{\link{Forecast}} objects in the passed model.
#' @param model a \code{Model} object
#' @export
forecasts <- function(model, ...) {
  UseMethod("forecasts")
}

#' @export
forecasts.default <- function(model, ...) {
  # unlike other resources that are a list of URLs, model each model forecast is a dict with three keys:
  #   'timezero_date', 'data_version_date', 'forecast'
  #
  # for example:
  # [{'timezero_date': '20170117', 'data_version_date': None, 'forecast': 'http://127.0.0.1:8000/api/forecast/35/'},
  #  {'timezero_date': '20170124', 'data_version_date': None, 'forecast': None}]}
  #
  # note that 'data_version_date' and 'forecast' might be None. in this method we only return Forecast objects
  # that are not None. (recall that a model's TimeZeros might not have associated forecast data yet.)
  forecasts_json <- model$json$forecasts  # some may have no forecast, so filter next
  cond <- sapply(forecasts_json, function(forecast_json) !is.null(forecast_json$forecast))
  forecasts_json <- forecasts_json[cond]
  forecasts <- lapply(forecasts_json,
    function(forecast_json) {
      new_forecast(model$zoltar_connection, forecast_json$forecast)
    })
  forecasts
}


#' get the forecast for an id
#'
#' Utility function that returns the \code{\link{Forecast}} object with the passed primary key (AKA id). Used for
#' example in the case where you've called \code{\link{upload_forecast}} to get an \code{\link{UploadFileJob}} object,
#' from which you then want to obtain the newly-uploaded Forecast (see the demo app) - once the upload is successfully
#' complete.
#'
#' @return a \code{\link{Forecast}} object
#' @param model a \code{Model} object
#' @param forecast_pk the id of a forecast on the host. for example, the id for the model with the url
#'   \emph{http://example.com/api/forecast/71/} is 71.
#' @export
forecast_for_pk <- function(model, forecast_pk, ...) {
  UseMethod("forecast_for_pk")
}

#' @export
forecast_for_pk.default <- function(model, forecast_pk, ...) {
  forecast_url <- paste0(model$zoltar_connection$host, '/api/forecast/', forecast_pk, '/')
  new_forecast(model$zoltar_connection, forecast_url)
}


#' upload a forecast
#'
#' Function submits a forecast file to the server for uploading. returns an \code{\link{UploadFileJob}} object that can
#' be used to track the upload's progress. (Uploads are processed in a queue, which means they are delayed until their
#' turn comes up, which depends on the number of current uploads in the queue.)
#'
#' @return the \code{\link{UploadFileJob}} object for the upload process
#' @param model a \code{Model} object
#' @param timezero_date the date of the project timezero you are uploading for. it is a string in format \code{YYYYMMDD}
#' @param forecast_csv_file a CSV file in the Zoltar standard format - see \url{https://www.zoltardata.com/docs#forecasts}
#' @export
upload_forecast <- function(model, timezero_date, forecast_csv_file, ...) {
  UseMethod("upload_forecast")
}


post_forecast <- function(model, forecast_csv_file, timezero_date) {
  # upload_forecast() helper that enables testing
  response <- httr::POST(url = paste0(model$url, 'forecasts/'),
                         add_headers("Authorization" = paste0("JWT ", model$zoltar_connection$session$token)),
                         body = list(data_file = upload_file(forecast_csv_file),
                                     timezero_date = timezero_date))
  stop_for_status(response)
  content(response, "parsed")
}


#' @export
upload_forecast.default <- function(model, timezero_date, forecast_csv_file, ...) {
  upload_file_job_json <- post_forecast(model, forecast_csv_file, timezero_date)
  new_upload_file_job(model$zoltar_connection, upload_file_job_json$url)  # throw away json and let refresh() reload it
}


#
# Forecast class - ZoltarResource subclass
#

#' make a new forecast (lower level function)
#'
#' Constructor that returns a new \code{\link{Forecast}} object for a particular forecast's url. Not normally called
#' directly.
#'
#' @return a \code{\link{Forecast}} object
#' @param zoltar_connection a \code{ZoltarConnection} object as returned by \code{\link{new_connection}}
#' @param url host address of the Forecast, e.g., \emph{http://example.com/api/forecast/71/}.
#' @export
new_forecast <- function(zoltar_connection, url) {
  self <- new_resource(zoltar_connection, url)
  class(self) <- append("Forecast", class(self))
  self
}


#' get a forecast's data
#'
#' @return forecast data associated with the passed \code{\link{Forecast}} object based on the requested format
#' @param is_json a boolean specifying whether the forecast is in JSON (the default) or CSV format
#' @export
data <- function(forecast, is_json=TRUE, ...) {
  UseMethod("data")
}

#' @export
data.default <- function(forecast, is_json=TRUE, ...) {
  data_url <- forecast$json$forecast_data
  if (is_json) {
    json_for_url(forecast$zoltar_connection, data_url)
  } else {  # CSV
    # todo fix api_views.forecast_data() to use proper accept type rather than 'format' query parameter
    response <- httr::GET(url = data_url,
                          add_headers("Authorization" = paste0("JWT ", forecast$zoltar_connection$session$token)),
                          query = list(format = "csv"))
    stop_for_status(response)
    content(response, encoding="UTF-8")
  }
}


#' get a forecast's timezero_date
#'
#' A \emph{getter} function that returns the timezero_date of the passed \code{\link{Forecast}}.
#'
#' @return string formatted as \code{YYYYMMDD}
#' @param forecast a \code{\link{Forecast}} object
#' @export
timezero_date <- function(forecast, ...) {
  UseMethod("timezero_date")
}

#' @export
timezero_date.default <- function(forecast, ...) {
  forecast$json$time_zero$timezero_date
}


#' get a forecast's csv_filename
#'
#' A \emph{getter} function that returns the csv_filename of the passed \code{\link{Forecast}}.
#'
#' @return filename string
#' @param forecast a \code{\link{Forecast}} object
#' @export
csv_filename <- function(forecast, ...) {
  UseMethod("csv_filename")
}

#' @export
csv_filename.default <- function(forecast, ...) {
  forecast$json$csv_filename
}


#
# UploadFileJob class - ZoltarResource subclass
#

new_upload_file_job <- function(zoltar_connection, url) {
  self <- new_resource(zoltar_connection, url)
  class(self) <- append("UploadFileJob", class(self))
  self
}


#' get an upload_file_job's int status as a string
#'
#' @return the passed \code{\link{UploadFileJob}}'s status as a human-readable string (rather than the internal int).
#'  the possible values are: "PENDING", "CLOUD_FILE_UPLOADED", "QUEUED", "CLOUD_FILE_DOWNLOADED", "SUCCESS", or
#'  "FAILED".
#' @param upload_file_job an \code{\link{UploadFileJob}} object
#' @export
status_as_str <- function(upload_file_job, ...) {
  UseMethod("status_as_str")
}

#' @export
status_as_str.default <- function(upload_file_job, ...) {
  # to map status ints to strings, we simply index into a vector. recall status starts with zero
  status_names <- c("PENDING", "CLOUD_FILE_UPLOADED", "QUEUED", "CLOUD_FILE_DOWNLOADED", "SUCCESS", "FAILED")
  status_int <- upload_file_job$json$status  # an integer
  status_str <- status_names[status_int + 1]
  status_str
}


#' get an upload_file_job's output_json
#'
#' A \emph{getter} function that returns the output_json field of the passed \code{\link{UploadFileJob}}. This JSON
#' is a 'catch call' data structure that contains output that varies according to the kind of upload that was done.
#' Currently the only use is when uploading a forecast via \code{\link{upload_forecast}}, which results in a
#' \code{forecast_pk} key of the new forecast.
#'
#' @return output_json field (JSON) as a string
#' @param upload_file_job a \code{\link{UploadFileJob}} object
#' @export
output_json <- function(upload_file_job, ...) {
  UseMethod("output_json")
}

#' @export
output_json.default <- function(upload_file_job, ...) {
  upload_file_job$json$output_json
}

