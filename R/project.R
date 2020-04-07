#
# ---- project functions ----
#

#' Create a project
#'
#' Creates the project using the passed project configuration list. Fails if a project with the passed name already
#'   exists.
#'
#' @return project_url of the newly-created project
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_config A `list` containing a Zoltar project configuration. note that this list validated by the
#'   server and not here. An example: cdc-project.json Full documentation at \url{https://docs.zoltardata.com/}.
#' @export
#' @examples \dontrun{
#'   new_project_url <- create_project(conn, jsonlite::read_json("cdc-project.json"))
#' }
create_project <- function(zoltar_connection, project_config) {
  re_authenticate_if_necessary(zoltar_connection)
  projects_url <- url_for_projects(zoltar_connection)
  response <- httr::POST(
    url = projects_url,
    add_auth_headers(zoltar_connection),
    body = list(project_config = project_config),
    encode="json")
  # the Zoltar API returns 400 if there was an error POSTing. the content is JSON with a $error key that contains the
  # error message
  json_response <- httr::content(response, "parsed")
  if (response$status_code == 400) {
    stop(json_response$error, call. = FALSE)
  }

  json_response$url  # throw away rest of json and let project_info() reload/refresh it
}


#' Delete a project
#'
#' Deletes the project with the passed URL. This is permanent and cannot be undone.
#'
#' @return None
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_url URL of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   delete_project(conn, "https://www.zoltardata.com/project/9/")
#' }
delete_project <- function(zoltar_connection, project_url) {
  delete_resource(zoltar_connection, project_url)
}


#' Get a project's scores
#'
#' @return A `data.frame` of score data for all models in the passed project URL
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_url URL of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_scores <- scores(conn, "https://www.zoltardata.com/project/9/")
#' }
scores <- function(zoltar_connection, project_url) {
  scores_url <- paste0(project_url, 'score_data/')
  get_resource(zoltar_connection, scores_url)
}


#' Get a project's truth
#'
#' @return A `data.frame` of truth data for the passed project URL
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_url URL of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_truth <- truth(conn, "https://www.zoltardata.com/project/9/")
#' }
truth <- function(zoltar_connection, project_url) {
  truth_url <- paste0(project_url, 'truth_data/')
  get_resource(zoltar_connection, truth_url)
}


#' Get a project's models
#'
#' @return A `data.frame` of model contents for all models in the passed project
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_url URL of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_models <- models(conn, "https://www.zoltardata.com/project/9/")
#' }
models <- function(zoltar_connection, project_url) {
  models_url <- paste0(project_url, 'models/')
  models_json <- get_resource(zoltar_connection, models_url)
  id_column <- c()            # integer
  url_column <- c()           # character
  project_url_column <- c()   # ""
  owner_url_column <- c()     # ""
  name_column <- c()          # ""
  abbreviation_column <- c()  # ""
  description_column <- c()   # ""
  home_url_column <- c()      # ""
  aux_data_url_column <- c()  # "". might be NULL. substitute NA if so
  for (model_json in models_json) {
    id_column <- append(id_column, model_json$id)
    url_column <- append(url_column, model_json$url)
    project_url_column <- append(project_url_column, model_json$project)

    owner_url_value <- if (is.null(model_json$owner)) NA else model_json$owner
    owner_url_column <- append(owner_url_column, owner_url_value)

    name_column <- append(name_column, model_json$name)
    abbreviation_column <- append(abbreviation_column, model_json$abbreviation)
    description_column <- append(description_column, model_json$description)
    home_url_column <- append(home_url_column, model_json$home_url)

    aux_data_value <- if (is.null(model_json$aux_data_url)) NA else model_json$aux_data_url
    aux_data_url_column <- append(aux_data_url_column, aux_data_value)
  }
  data.frame(id = id_column, url = url_column, project_url = project_url_column, owner_url = owner_url_column,
             name = name_column, description = description_column, home_url = home_url_column,
             aux_data_url = aux_data_url_column, stringsAsFactors = FALSE)
}


#' Get a project's zoltar_units
#'
#' @return A `data.frame` of unit contents for the passed project
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_url URL of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_units <- zoltar_units(conn, "https://www.zoltardata.com/project/9/")
#' }
zoltar_units <- function(zoltar_connection, project_url) {
  units_url <- paste0(project_url, 'units/')
  units_json <- get_resource(zoltar_connection, units_url)
  id_column <- c()                    # integer
  url_column <- c()                   # character
  name_column <- c()                  # ""
  for (unit_json in units_json) {
    id_column <- append(id_column, unit_json$id)
    url_column <- append(url_column, unit_json$url)
    name_column <- append(name_column, unit_json$name)
  }
  data.frame(id = id_column, url = url_column, name = name_column, stringsAsFactors = FALSE)
}


#' Get a project's targets
#'
#' @return A `data.frame` of target contents for the passed project
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_url URL of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_targets <- targets(conn, "https://www.zoltardata.com/project/9/")
#' }
targets <- function(zoltar_connection, project_url) {
  targets_url <- paste0(project_url, 'targets/')
  targets_json <- get_resource(zoltar_connection, targets_url)
  data_frame_from_targets_json(targets_json)
}


data_frame_from_targets_json <- function(targets_json) {
  # helper that can be called independently
  id_column <- c()                    # integer
  url_column <- c()                   # character
  name_column <- c()                  # ""
  description_column <- c()           # ""
  type_column <- c()                  # ""
  is_step_ahead_column <- c()         # logical
  step_ahead_increment_column <- c()  # numeric (NULL if not is_step_ahead)
  unit_column <- c()                  # character (NULL for some target types)
  for (target_json in targets_json) {
    id_column <- append(id_column, target_json$id)
    url_column <- append(url_column, target_json$url)
    name_column <- append(name_column, target_json$name)
    description_column <- append(description_column, target_json$description)
    type_column <- append(type_column, target_json$type)
    is_step_ahead_column <- append(is_step_ahead_column, target_json$is_step_ahead)

    step_ahead_value <- if (is.null(target_json$step_ahead_increment)) NA else target_json$step_ahead_increment
    step_ahead_increment_column <- append(step_ahead_increment_column, step_ahead_value)

    unit_value <- if (is.null(target_json$unit)) NA else target_json$unit
    unit_column <- append(unit_column, unit_value)
  }
  data.frame(id = id_column, url = url_column, name = name_column, description = description_column,
             type = type_column, is_step_ahead = is_step_ahead_column,
             step_ahead_increment = step_ahead_increment_column, unit = unit_column, stringsAsFactors = FALSE)
}


#' Get a project's timezeros
#'
#' @return A `data.frame` of timezero contents for the passed project
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_url URL of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_timezeros <- timezeros(conn, "https://www.zoltardata.com/project/9/")
#' }
timezeros <- function(zoltar_connection, project_url) {
  timezeros_url <- paste0(project_url, 'timezeros/')
  timezeros_json <- get_resource(zoltar_connection, timezeros_url)
  id_column <- c()                    # integer
  url_column <- c()                   # character
  timezero_date_column <- c()         # Date
  data_version_date_column <- c()     # "" (might be NULL)
  is_season_start_column <- c()       # logical
  season_name_column <- c()           # character (might be NULL)
  for (timezero_json in timezeros_json) {
    id_column <- append(id_column, timezero_json$id)
    url_column <- append(url_column, timezero_json$url)
    timezero_date_column <- append(timezero_date_column, as.Date(timezero_json$timezero_date, YYYY_MM_DD_DATE_FORMAT))

    data_version_date_value <- if (is.null(timezero_json$data_version_date)) NA else
      as.Date(timezero_json$data_version_date, YYYY_MM_DD_DATE_FORMAT)
    data_version_date_column <- append(data_version_date_column, data_version_date_value)

    is_season_start_column <- append(is_season_start_column, timezero_json$is_season_start)

    season_name_value <- if (is.null(timezero_json$season_name)) NA else timezero_json$season_name
    season_name_column <- append(season_name_column, season_name_value)
  }
  data.frame(id = id_column, url = url_column, timezero_date = timezero_date_column,
             data_version_date = data_version_date_column, is_season_start = is_season_start_column,
             season_name = season_name_column, stringsAsFactors = FALSE)
}


#
# ---- info functions ----
#

#' Get information about a project
#'
#' @return A `list` of project information for the passed project_url
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param project_url URL of a project in zoltar_connection's projects
#' @export
#' @examples \dontrun{
#'   the_project_info <- project_info(conn, "https://www.zoltardata.com/project/9/")
#' }
project_info <- function(zoltar_connection, project_url) {
  get_resource(zoltar_connection, project_url)
}


#' Get information about a target
#'
#' @return A `list` of target information for the passed target_url
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param target_url URL of a target in zoltar_connection's targets
#' @export
#' @examples \dontrun{
#'   the_target_info <- target_info(conn, "https://www.zoltardata.com/target/3/")
#' }
target_info <- function(zoltar_connection, target_url) {
  get_resource(zoltar_connection, target_url)
}


#' Get information about a timezero
#'
#' @return A `list` of timezero information for the passed timezero_url
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param timezero_url URL of a timezero in zoltar_connection's timezeros
#' @export
#' @examples \dontrun{
#'   the_timezero_info <- timezero_info(conn, "https://www.zoltardata.com/timezero/3/")
#' }
timezero_info <- function(zoltar_connection, timezero_url) {
  get_resource(zoltar_connection, timezero_url)
}


#' Get information about a unit
#'
#' @return A `list` of unit information for the passed unit_url
#' @param zoltar_connection A `ZoltarConnection` object as returned by \code{\link{new_connection}}
#' @param unit_url URL of a unit in zoltar_connection's zoltar_units
#' @export
#' @examples \dontrun{
#'   the_unit_info <- unit_info(conn, "https://www.zoltardata.com/unit/3/")
#' }
unit_info <- function(zoltar_connection, unit_url) {
  get_resource(zoltar_connection, unit_url)
}

