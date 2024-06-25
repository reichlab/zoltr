CDC_POINT_ROW_TYPE <- "Point"  # incoming cdc.csv file row type
POINT_PREDICTION_CLASS <- "point"  # JSON prediction class for point prediction elements
BIN_PREDICTION_CLASS <- "bin"  # "" bin ""


#' Loads and converts a CDC CSV file to Zoltar's native `list` format
#'
#' @return cdc_csv_file's data as Zoltar's native `list` format, but only the "predictions" item, and not "meta"
#' @param season_start_year An integer specifying the "season" that cdc_csv_file is in. Used to convert EWs to
#'   YYYY_MM_DD_DATE_FORMAT. \pkg{zoltr} uses week 30 as the season breakpoint, e.g. the "2016/2017 season" starts with
#    EW30-2016 (EWs 30 through 52/53) and ends with EW29-2017 (EWs 01 through 29).
#' @param cdc_csv_file A CDC CSV file
#' @export
#' @examples \dontrun{
#'   forecast_data <- forecast_data_from_cdc_csv_file(2016, "my_forecast.cdc.csv")
#' }
forecast_data_from_cdc_csv_file <- function(season_start_year, cdc_csv_file) {
  cdc_data_frame <- utils::read.csv(cdc_csv_file, stringsAsFactors = FALSE)  # "NA" -> NA
  forecast_data_from_cdc_data_frame(season_start_year, cdc_data_frame)
}


#
# Recall the seven cdc-project.json targets and their types:
# -------------------------+-------------------------------+--------------------------+-----------+----------------
# Target name              | target_type                   | outcome_variable         | data_type | numeric_horizon
# -------------------------+-------------------------------+--------------------------+-----------+----------------
# "Season onset"           | Target.NOMINAL_TARGET_TYPE    | "season onset"           | date      | n/a
# "Season peak week"       | Target.DATE_TARGET_TYPE       | "season peak week"       | text      | n/a
# "Season peak percentage" | Target.CONTINUOUS_TARGET_TYPE | "season peak percentage" | float     | n/a
# "1 wk ahead"             | Target.CONTINUOUS_TARGET_TYPE | "ILI percent"            | float     | 1
# "2 wk ahead"             | ""                            | ""                       | ""        | 2
# "3 wk ahead"             | ""                            | ""                       | ""        | 3
# "4 wk ahead"             | ""                            | ""                       | ""        | 4
# -------------------------+-------------------------------+--------------------------+-----------+----------------
#
# Note that the "Season onset" target is nominal and not date. This is due to how the CDC decided to represent the
# case when predicting no season onset, i.e., the threshold is not exceeded. This is done via a "none" bin where
# both Bin_start_incl and Bin_end_notincl are the strings "none" and not an EW week number. Thus, we have to store
# all bin starts as strings and not dates. At one point the lab was going to represent this case by splitting the
# "Season onset" target into two: "season_onset_binary" (a Target.BINARY that indicates whether there is an onset or
# not) and "season_onset_date" (a Target.DATE_TARGET_TYPE that is the onset date if "season_onset_binary" is true).
# But we dropped that idea and stayed with the original single nominal target.
#


#' [forecast_data_from_cdc_csv_file()] helper
#'
#' @return same as [forecast_data_from_cdc_csv_file()]
#' @param season_start_year as passed to [forecast_data_from_cdc_csv_file()]
#' @param cdc_data_frame ""
#' @importFrom rlang .data
forecast_data_from_cdc_data_frame <- function(season_start_year, cdc_data_frame) {  # testable internal function that does the work
  names(cdc_data_frame) <- sapply(names(cdc_data_frame), tolower)

  # validate cdc_data_frame
  if (!(inherits(cdc_data_frame, "data.frame"))) {
    stop("cdc_data_frame was not a `data.frame`", call. = FALSE)
  }

  if ((length(cdc_data_frame) == 0) || !all(names(cdc_data_frame) != c("Location", "Target", "Type", "Unit",
                                                                       "Bin_start_incl", "Bin_end_notincl", "Value"))) {
    stop("cdc_data_frame did not have required columns", call. = FALSE)
  }

  predictions <- list()
  cdc_data_frame_grouped <- cdc_data_frame %>%
    dplyr::group_by(.data[["location"]], .data[["target"]], .data[["type"]]) %>%
    dplyr::group_data()
  for (group_idx in seq_len(nrow(cdc_data_frame_grouped))) {
    group_row <- cdc_data_frame_grouped[group_idx,]  # group_row$location,  group_row$target,  group_row$type
    if (!group_row$target %in% c("Season onset", "Season peak week", "Season peak percentage",
                                 "1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")) {
      stop("invalid target_name: '", group_row$target, "'", call. = FALSE)
    }

    point_values <- list()  # NB: should only be one point row, but collect all (but don't validate here)
    bincat_cats <- list()
    bincat_probs <- list()
    for (group_rows_idx in seq_along(group_row$.rows[[1]])) {
      cdc_data_frame_idx <- group_row$.rows[[1]][group_rows_idx]
      # NB: cdc_row values could come in as numbers or strings, depending on the source csv file values
      cdc_row <- cdc_data_frame[cdc_data_frame_idx,]  # cdc_row$bin_start_incl, cdc_row$bin_end_notincl, cdc_row$value
      if (group_row$type == CDC_POINT_ROW_TYPE) {
        point_value <- process_csv_point_row(season_start_year, group_row$target, as.numeric(cdc_row$value))
        point_values <- append(point_values, point_value)
      } else {  # bin row
        # recall that the "Season onset" target is nominal and not date. This is due to how the CDC decided to represent
        # the case when predicting no season onset, i.e., the threshold is not exceeded. This is done via a "none" bin
        # where both Bin_start_incl and Bin_end_notincl are the strings "none" and not an EW week number. Thus we need
        # to check for that case and replace with NAs, which is what process_csv_bin_row() expects
        bin_start_incl <- if (cdc_row$bin_start_incl == "none") as.numeric(NA) else as.numeric(cdc_row$bin_start_incl)
        bin_end_notincl <- if (cdc_row$bin_end_notincl == "none") as.numeric(NA) else as.numeric(cdc_row$bin_end_notincl)
        bin_cat_and_prob <- process_csv_bin_row(season_start_year, group_row$target, as.numeric(cdc_row$value),
                                                bin_start_incl, bin_end_notincl)
        bincat_cats <- append(bincat_cats, bin_cat_and_prob[[1]])
        bincat_probs <- append(bincat_probs, bin_cat_and_prob[[2]])
      }
    }

    # add the actual prediction dicts
    if (length(point_values) > 0) {  # yes warning
      if (length(point_values) > 1) {
        stop("length(point_values) > 1: ", point_values, call. = FALSE)
      }

      point_value <- point_values[[1]]
      prediction <- list("unit" = group_row$location, "target" = group_row$target, "class" = POINT_PREDICTION_CLASS,
                         "prediction" = list("value" = point_value))
      predictions[[length(predictions) + 1]] <- prediction
    }
    if (length(bincat_cats) >= 1) {  # yes warning: "NAs introduced by coercion"
      prediction <- list("unit" = group_row$location, "target" = group_row$target, "class" = BIN_PREDICTION_CLASS,
                         "prediction" = list("cat" = bincat_cats, "prob" = bincat_probs))
      predictions[[length(predictions) + 1]] <- prediction
    }
  }

  list("predictions" = predictions)
}


process_csv_point_row <- function(season_start_year, target_name, value) {
  # returns: point value for the args
  if (target_name == 'Season onset') {  # nominal target. value: None or an EW Monday date
    if (is.na(value)) {
      'none'        # convert back from None to original 'none' input
    } else {  # value is an EW week number (float)
      # note that value may be a fraction (e.g., 50.0012056690978, 4.96302456525203), so we round
      # the EW number to get an int, but this could cause boundary issues where the value is
      # invalid, either:
      #   1) < 1 (so use last EW in season_start_year), or:
      #   2) > the last EW in season_start_year (so use EW01 of season_start_year + 1)
      ew_week <- round(value)
      if (ew_week < 1) {
        ew_week <- mmwr_weeks_in_year(season_start_year)  # wrap back to previous EW
      } else if (ew_week > mmwr_weeks_in_year(season_start_year)) {
        ew_week <- 1
      }
      monday_date <- monday_date_from_ew_and_season_start_year(ew_week, season_start_year)
      strftime(monday_date, YYYY_MM_DD_DATE_FORMAT)
    }
  } else if (is.na(value)) {
    stop("None point values are only valid for 'Season onset' targets. target_name='", target_name, "'",
         call. = FALSE)
  } else if (target_name == 'Season peak week') {  # date target. value: an EW Monday date
    # same 'wrapping' logic as above to handle rounding boundaries
    ew_week <- round(value)
    if (ew_week < 1) {
      ew_week <- mmwr_weeks_in_year(season_start_year)  # wrap back to previous EW
    } else if (ew_week > mmwr_weeks_in_year(season_start_year)) {
      ew_week <- 1
    }
    monday_date <- monday_date_from_ew_and_season_start_year(ew_week, season_start_year)
    strftime(monday_date, YYYY_MM_DD_DATE_FORMAT)
  } else {  # 'Season peak percentage', '1 wk ahead', '2 wk ahead', '3 wk ahead', '4 wk ahead'
    value
  }
}


process_csv_bin_row <- function(season_start_year, target_name, value, bin_start_incl, bin_end_notincl) {
  # returns: 2-tuple for the args: (bin_cat, bin_prob)
  if (target_name == 'Season onset') {  # nominal target. value: None or an EW Monday date
    if (is.na(bin_start_incl) && is.na(bin_end_notincl)) {  # "none" bin (probability of no onset)
      list('none', value)  # convert back from None to original 'none' input
    } else if (!is.na(bin_start_incl) && !is.na(bin_end_notincl)) {  # regular (non-"none") bin
      monday_date <- monday_date_from_ew_and_season_start_year(bin_start_incl, season_start_year)
      list(strftime(monday_date, YYYY_MM_DD_DATE_FORMAT), value)
    } else {
      stop("got 'Season onset' row but not both start and end were None. bin_start_incl=", bin_start_incl,
           ", bin_end_notincl=", bin_end_notincl,
           call. = FALSE)
    }
  } else if (is.na(bin_start_incl) || is.na(bin_end_notincl)) {
    stop("None bins are only valid for 'Season onset' targets. target_name='", target_name, "', ",
         ". bin_start_incl, bin_end_notincl: ", bin_start_incl, ", ", bin_end_notincl,
         call. = FALSE)
  } else if (target_name == 'Season peak week') {  # date target. value: an EW Monday date
    monday_date <- monday_date_from_ew_and_season_start_year(bin_start_incl, season_start_year)
    list(strftime(monday_date, YYYY_MM_DD_DATE_FORMAT), value)
  } else {  # 'Season peak percentage', '1 wk ahead', '2 wk ahead', '3 wk ahead', '4 wk ahead'
    list(bin_start_incl, value)
  }
}


#
# ---- MMWR utils ----
#

YYYY_MM_DD_DATE_FORMAT <- '%Y-%m-%d'  # e.g., '2017-01-17'


# This number is the internal reichlab standard: "We used week 30. I don't think this is a standardized concept outside
# of our lab though. We use separate concepts for a "season" and a "year". So, e.g. the "2016/2017 season" starts with
# EW30-2016 and ends with EW29-2017."
SEASON_START_EW_NUMBER <- 30


monday_date_from_ew_and_season_start_year <- function(ew_week, season_start_year) {
  # :param ew_week: an epi week from within a cdc csv forecast file. e.g., 1, 30, 52
  # :param season_start_year
  # :return: a datetime.date that is the Monday of the EW corresponding to the args
  if (ew_week < SEASON_START_EW_NUMBER) {
    sunday_date <- MMWRweek::MMWRweek2Date(season_start_year + 1, ew_week)
  } else {
    sunday_date <- MMWRweek::MMWRweek2Date(season_start_year, ew_week)
  }
  sunday_date + 1  # add one day
}


mmwr_weeks_in_year <- function(year) {
  # returns the number of epiweeks in a year. based on `pymmwr.epiweeks_in_year()` - https://github.com/reichlab/pymmwr/blob/b5ebdd88cc1e4d33548010e04b25ece4cb982b8e/pymmwr.py#L83
  if (MMWRweek::MMWRweek(MMWRweek::MMWRweek2Date(year, 53))$MMWRyear == year) {
    53
  } else {
    52
  }
}
