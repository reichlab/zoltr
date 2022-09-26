#
# ---- utility functions ----
#

url_for_projects <- function(zoltar_connection) {
  paste0(zoltar_connection$host, '/api/projects/')
}


url_for_token_auth <- function(zoltar_connection) {
  paste0(zoltar_connection$host, '/api-token-auth/')
}


add_auth_headers <- function(zoltar_connection) {
  if (!inherits(zoltar_connection, "ZoltarConnection")) {
    stop("zoltar_connection was not a ZoltarConnection: '", zoltar_connection, "'", call. = FALSE)
  }

  if (inherits(zoltar_connection$session, "ZoltarSession")) {
    httr::add_headers("Authorization" = paste0("JWT ", zoltar_connection$session$token))
  }
}


#' Get JSON for a resource (URL). Authenticates if necessary
#'
#' @return A `list` that contains JSON information for the passed URL
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @param url A string of the resource's URL
#' @param col_types Same as readr::read_csv takes
get_resource <- function(zoltar_connection, url, col_types = NULL) {
  re_authenticate_if_necessary(zoltar_connection)
  message("get_resource(): GET: ", url)
  response <- httr::GET(url = url, add_auth_headers(zoltar_connection))
  httr::stop_for_status(response)
  if (httr::http_type(response) == "application/json") {
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  } else if (httr::http_type(response) == "text/csv") {
    readr::read_csv(httr::content(response, "raw"), col_types = col_types,
                    locale = readr::locale(encoding = "UTF-8"))
  } else {
    stop("un-handled content type: '", httr::http_type(response), "'", call. = FALSE)
  }
}


# deletes the resource at the passed URL
delete_resource <- function(zoltar_connection, url) {
  re_authenticate_if_necessary(zoltar_connection)
  message("delete_resource(): DELETE: ", url)
  response <- httr::DELETE(url = url, add_auth_headers(zoltar_connection))
  httr::stop_for_status(response)
  response
}


#
# ---- ZoltarConnection class ----
#

#' Get a connection to a Zoltar host
#'
#' Returns a new connection object, which is the starting point for working with the Zoltar API. Once you have the
#' connection you can call [zoltar_authenticate()] on it, and then call [projects()] to get a list
#' of Project objects to start working with.
#'
#' A note on URLs: We require a trailing slash ('/') on all URLs. The only exception is the host arg passed to this
#' function. This convention matches Django REST framework one, which is what Zoltar is written in.
#'
#' @return A `ZoltarConnection` object
#' @param host The Zoltar site to connect to. Does *not* include a trailing slash ('/'). Defaults to <https://zoltardata.com>
#' @export
#' @examples \dontrun{
#'   conn <- new_connection()
#' }

new_connection <- function(host = "https://zoltardata.com") {
  self <- structure(environment(), class = "ZoltarConnection")
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
        "\n", sep = '')
  }


#' Log in to a Zoltar host
#'
#' Returns a new `ZoltarConnection` object, which is the starting point for working with the Zoltar API.
#' Once you have the connection you can call [zoltar_authenticate()] on it, and call [projects()] to get a list of objects
#' to start working with.
#'
#' @return None
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()].
#' @param username Username for the account to use on the connection's host
#' @param password Password ""
#' @export
#' @examples \dontrun{
#'   zoltar_authenticate(conn, "USERNAME", "PASSWORD")
#' }
zoltar_authenticate <- function(zoltar_connection, username, password) {
  if (!inherits(zoltar_connection, "ZoltarConnection")) {
    stop("zoltar_connection was not a ZoltarConnection: '", zoltar_connection, "'", call. = FALSE)
  }

  zoltar_connection$username <- username
  zoltar_connection$password <- password
  zoltar_connection$session <- new_session(zoltar_connection)
}


re_authenticate_if_necessary <- function(zoltar_connection) {
  if (!inherits(zoltar_connection, "ZoltarConnection")) {
    stop("zoltar_connection was not a ZoltarConnection: '", zoltar_connection, "'", call. = FALSE)
  }

  if (inherits(zoltar_connection$session, "ZoltarSession") && is_token_expired(zoltar_connection$session)) {
    message("re-authenticating expired token '", zoltar_connection$host, "'")
    zoltar_authenticate(zoltar_connection, zoltar_connection$username, zoltar_connection$password)
  }
}


#' Get information about all projects
#'
#' @return A `data.frame` of all projects' contents
#' @param zoltar_connection A `ZoltarConnection` object as returned by [new_connection()]
#' @export
#' @examples \dontrun{
#'   the_projects <- projects(conn)
#' }
projects <- function(zoltar_connection) {
  projects_json <- get_resource(zoltar_connection, url_for_projects(zoltar_connection))
  id_column <- c()                     # integer
  url_column <- c()                    # character
  owner_url_column <- c()              # ""
  is_public_column <- c()              # logical
  name_column <- c()                   # character
  description_column <- c()            # ""
  home_url_column <- c()               # ""
  core_data_column <- c()              # ""
  for (project_json in projects_json) {
    id_column <- append(id_column, project_json$id)
    url_column <- append(url_column, project_json$url)

    owner_url_value <- if (is.null(project_json$owner)) NA else project_json$owner
    owner_url_column <- append(owner_url_column, owner_url_value)

    is_public_column <- append(is_public_column, project_json$is_public)
    name_column <- append(name_column, project_json$name)
    description_column <- append(description_column, project_json$description)
    home_url_column <- append(home_url_column, project_json$home_url)
    core_data_column <- append(core_data_column, project_json$core_data)
  }
  data.frame(id = id_column, url = url_column, owner_url = owner_url_column, public = is_public_column, name = name_column,
             description = description_column, home_url = home_url_column, core_data = core_data_column,
             stringsAsFactors = FALSE)
}


#
# ZoltarSession class. used internally only
#

new_session <- function(zoltar_connection) {
  self <- structure(environment(), class = "ZoltarSession")
  zoltar_connection <- zoltar_connection
  token <- get_token(self)  # expects zoltar_connection
  self
}


# POSTs to obtain and return a new JWT token string from zoltar. it has decoded contents that look like this:
# - header:  {"typ": "JWT", "alg": "HS256"}
# - payload: {"user_id": 3, "username": "model_owner1", "exp": 1558442805, "email": ""}
get_token <- function(zoltar_session) {
  zoltar_connection <- zoltar_session$zoltar_connection
  token_auth_url <- url_for_token_auth(zoltar_connection)
  message("get_token(): POST: ", token_auth_url)
  response <-
    httr::POST(
      url = token_auth_url,
      httr::accept_json(),
      body = list(
        username = zoltar_connection$username,
        password = zoltar_connection$password
      )
    )
  httr::stop_for_status(response)
  json_content <- httr::content(response, "parsed")
  json_content$token
}


# returns a POSIXct for the zoltar_session's token. see notes in is_token_expired() for details on extracting the date
token_expiration_date <- function(zoltar_session) {
  token_split <- strsplit(zoltar_session$token, ".", fixed = TRUE)  # 3 parts: header, payload, and signature
  payload_encoded <- token_split[[1]][2]
  payload_decoded <- base64url::base64_urldecode(payload_encoded)
  payload <- jsonlite::fromJSON(payload_decoded)
  exp_timestamp_utc <- payload$exp
  exp_timestamp_date <- .POSIXct(exp_timestamp_utc, tz = "UTC")
  exp_timestamp_date
}


# returns TRUE if zoltar_session's token is expired, and FALSE if still valid. details: based on how Zoltar implements
# JWT, we determine expiration by comparing the current datetime to the token's payload's "exp" field. its value is a
# POSIX timestamp of a UTC date and time as returned by datetime.utcnow().timestamp() - https://docs.python.org/3.6/library/datetime.html#datetime.datetime.utcnow . xx
is_token_expired <- function(zoltar_session) {
  token_expiration_date(zoltar_session) <= Sys.time()  # now
}
