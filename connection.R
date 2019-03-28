library(httr)
library(jsonlite)


#
# ZoltarConnection class
#

new_connection <- function(host = "zoltardata.com") {
  # todo validate host - not NULL, proper URI structure
  
  self = structure(environment(), class = "ZoltarConnection")
  
  host <- host
  username <- NULL
  password <- NULL
  session <- NULL
  
  self
}


print.ZoltarConnection <- function(zoltar_connection, ...) {
  cat(
    class(zoltar_connection),
    " ",
    zoltar_connection$host,
    ". username, password=",
    if (is.null(zoltar_connection$username))
      "<no username>"
    else
      zoltar_connection$username,
    ", ",
    if (is.null(zoltar_connection$password))
      "<no password>"
    else
      zoltar_connection$password,
    ". session=",
    if (is.null(zoltar_connection$session))
      "<no session>"
    else
      zoltar_connection$session$token,
    "\n",
    sep = ''
  )
}


z_authenticate <-
  function(zoltar_connection,
           username,
           password,
           ...) {
    UseMethod("z_authenticate")
  }

z_authenticate.default <-
  function(zoltar_connection,
           username,
           password,
           ...) {
    zoltar_connection$username <- username
    zoltar_connection$password <- password
    zoltar_connection$session <- new_session(zoltar_connection)
  }


json_for_uri <- function(zoltar_connection, uri, ...) {
  # private
  UseMethod("json_for_uri")
}

json_for_uri.default <- function(zoltar_connection, uri, ...) {
  stopifnot(is(zoltar_connection$session, "ZoltarSession"))
  
  response <- httr::GET(
    url = uri,
    accept_json(),
    add_headers(
      "Accept" = "application/json; indent=4",
      "Authorization" = paste0("JWT ", zoltar_connection$session$token)
    )
  )
  stop_for_status(response)
  
  json_content <- content(response, "parsed")
  json_content
}


projects <- function(zoltar_connection, ...) {
  UseMethod("projects")
}

projects.default <- function(zoltar_connection, ...) {
  projects_json <-
    json_for_uri(zoltar_connection,
                 paste0(zoltar_connection$host, '/api/projects/'))
  projects <- lapply(projects_json,
                     function(project_json) {
                       new_project(zoltar_connection, project_json$url)
                     })
  projects
}


#
# ZoltarSession class. used internally only
#

new_session <- function(zoltar_connection) {
  # private
  self = structure(environment(), class = "ZoltarSession")
  
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
      url = paste0(zoltar_session$zoltar_connection$host, '/api-token-auth/'),
      accept_json(),
      body = list(
        username = zoltar_session$zoltar_connection$username,
        password = zoltar_session$zoltar_connection$password
      )
    )
  json_content <- content(response, "parsed")
  token <- json_content$token
  token
}


#
# ZoltarResource abstract class. used internally only
#

new_resource <- function(zoltar_connection, uri) {
  # todo validate uri - not NULL, proper URI structure
  
  self = structure(environment(), class = "ZoltarResource")
  
  zoltar_connection <- zoltar_connection
  uri <- uri
  json = NULL
  
  refresh(self)  # update json
  
  self
}


print.ZoltarResource <- function(zoltar_resource, ...) {
  cat(
    class(zoltar_resource)[1],
    " ",
    zoltar_resource$uri,
    " ",
    if (is.null(zoltar_resource$json))
      "<no JSON>"
    else
      paste0("len=", length(zoltar_resource$json)),
    "\n",
    sep = ''
  )
}


refresh <- function(zoltar_resource, ...) {
  UseMethod("refresh")
}

refresh.default <- function(zoltar_resource, ...) {
  zoltar_resource$json <-
    json_for_uri(zoltar_resource$zoltar_connection, zoltar_resource$uri)
}


delete <- function(zoltar_resource, ...) {
  UseMethod("delete")
}

delete.default <- function(zoltar_resource, ...) {
  response <- httr::DELETE(
    url = zoltar_resource$uri,
    accept_json(),
    add_headers(
      "Accept" = "application/json; indent=4",
      "Authorization" = paste0("JWT ", zoltar_resource$zoltar_connection$session$token)
    )
  )
  stop_for_status(response)  # HTTP_204_NO_CONTENT
}


id <- function(zoltar_resource, ...) {
  UseMethod("id")
}

id.default <- function(zoltar_resource, ...) {
  zoltar_resource$json$id
}


#
# Project class - ZoltarResource subclass
#

new_project <- function(zoltar_connection, uri) {
  self = new_resource(zoltar_connection, uri)
  class(self) <- append("Project", class(self))
  self
}


models <- function(project, ...) {
  UseMethod("models")
}

models.default <- function(project, ...) {
  models_uris <- project$json$models
  models <- lapply(models_uris,
                   function(model_uri) {
                     new_model(project$zoltar_connection, model_uri)
                   })
  models
}


name <- function(project, ...) {
  UseMethod("name")
}

name.default <- function(project, ...) {
  project$json$name
}


#
# Model class - ZoltarResource subclass
#

new_model <- function(zoltar_connection, uri) {
  self = new_resource(zoltar_connection, uri)
  class(self) <- append("Model", class(self))
  self
}


forecasts <- function(model, ...) {
  UseMethod("forecasts")
}

forecasts.default <- function(model, ...) {
  # unlike other resources that are a list of URIs, model each model forecast is a dict with three keys:
  #   'timezero_date', 'data_version_date', 'forecast'
  #
  # for example:
  # [{'timezero_date': '20170117', 'data_version_date': None, 'forecast': 'http://127.0.0.1:8000/api/forecast/35/'},
  #  {'timezero_date': '20170124', 'data_version_date': None, 'forecast': None}]}
  #
  # note that 'data_version_date' and 'forecast' might be None. in this method we only return Forecast objects
  # that are not None. (recall that a model's TimeZeros might not have associated forecast data yet.)
  
  forecasts_json <-
    model$json$forecasts  # some may have no forecast, so filter next
  cond <-
    sapply(forecasts_json, function(forecast_json)
      ! is.null(forecast_json$forecast))
  forecasts_json <- forecasts_json[cond]
  forecasts <- lapply(forecasts_json,
                      function(forecast_json) {
                        new_forecast(model$zoltar_connection, forecast_json$forecast)
                      })
  forecasts
}


forecast_for_pk <- function(model, forecast_pk, ...) {
  UseMethod("forecast_for_pk")
}

forecast_for_pk.default <- function(model, forecast_pk, ...) {
  forecast_uri = paste0(model$zoltar_connection$host,
                        '/api/forecast/',
                        forecast_pk,
                        '/')
  new_forecast(model$zoltar_connection, forecast_uri)
}


upload_forecast <-
  function(model,
           timezero_date,
           forecast_csv_file,
           ...) {
    UseMethod("upload_forecast")
  }

upload_forecast.default <-
  function(model,
           timezero_date,
           forecast_csv_file,
           ...) {
    response <- httr::POST(
      url = paste0(model$uri, 'forecasts/'),
      add_headers(
        "Authorization" = paste0("JWT ", model$zoltar_connection$session$token)
      ),
      body = list(
        data_file = upload_file(forecast_csv_file),
        timezero_date = timezero_date
      )
    )
    stop_for_status(response)
    json_content <- content(response, "parsed")
    new_upload_file_job(model$zoltar_connection, json_content$url)  # throw away json and let refresh() reload it
  }


#
# Forecast class - ZoltarResource subclass
#

new_forecast <- function(zoltar_connection, uri) {
  self = new_resource(zoltar_connection, uri)
  class(self) <- append("Forecast", class(self))
  self
}


data <- function(forecast, is_json = True, ...) {
  UseMethod("data")
}

data.default <- function(forecast, is_json = True, ...) {
  NULL  # todo
}


timezero_date <- function(forecast, ...) {
  UseMethod("timezero_date")
}

timezero_date.default <- function(forecast, ...) {
  forecast$json$time_zero$timezero_date
}


csv_filename <- function(forecast, ...) {
  UseMethod("csv_filename")
}

csv_filename.default <- function(forecast, ...) {
  forecast$json$csv_filename
}


#
# UploadFileJob class - ZoltarResource subclass
#

new_upload_file_job <- function(zoltar_connection, uri) {
  self = new_resource(zoltar_connection, uri)
  class(self) <- append("UploadFileJob", class(self))
  self
}


status_as_str <- function(upload_file_job, ...) {
  UseMethod("status_as_str")
}

status_as_str.default <- function(upload_file_job, ...) {
  # to map status ints to strings, we simply index into a vector. recall status starts with zero
  status_names <-
    c(
      'PENDING',
      'CLOUD_FILE_UPLOADED',
      'QUEUED',
      'CLOUD_FILE_DOWNLOADED',
      'SUCCESS',
      'FAILED'
    )
  status_int <- upload_file_job$json$status  # an integer
  status_str <- status_names[status_int + 1]
  status_str
}


output_json <- function(upload_file_job, ...) {
  UseMethod("output_json")
}

output_json.default <- function(upload_file_job, ...) {
  upload_file_job$json$output_json
}
