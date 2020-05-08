#' Converts forecast data from Zoltar's native `list` format to a `data.frame`
#'
#' @return A `data.frame` from forecast_data in zoltar-specific format. The 'class' of each row is
#'   as described in https://docs.zoltardata.com/datamodel/ and is one of: 'bin', 'named', 'point',
#'   'sample', 'quantile'. The columns are: 'unit', 'target', 'class', 'value', 'cat', 'prob',
#'   'sample', 'quantile', 'family', 'param1', 'param2', 'param3'. Note that the frame is 'sparse':
#'   not every row uses all columns, and unused ones are empty (''). However, the first four
#'   columns are always non-empty, i.e., every prediction has them. NB: columns are all text (i.e.,
#'   data type information from forecast_data is lost)
#' @param forecast_data Forecast data as a `list` in the Zoltar standard format
#' @export
#' @examples \dontrun{
#'   forecast_data <- jsonlite::read_json("docs-predictions.json")
#'   data_frame <- data_frame_from_forecast_data(forecast_data)
#' }
data_frame_from_forecast_data <- function(forecast_data) {
  if (!(inherits(forecast_data, "list"))) {
    stop("forecast_data was not a `list`", call. = FALSE)
  }

  rows <- list()  # list of lists, one per cvs row. columns as above
  for (prediction_element_idx in seq_along(forecast_data$predictions)) {
    prediction_element <- forecast_data$predictions[[prediction_element_idx]]
    unit_val <- prediction_element$unit
    target_val <- prediction_element$target
    prediction_class <- prediction_element$class
    prediction <- prediction_element$prediction
    value_val <- cat_val <- prob_val <- sample_val <- quantile_val <- family_val <- param1_val <- param2_val <- param3_val <- NA
    if (prediction_element$class == "bin") {
      for (cat_prob_idx in seq_along(prediction$cat)) {
        cat_val <- prediction$cat[[cat_prob_idx]]
        prob_val <- prediction$prob[[cat_prob_idx]]
        pred_row <- list(unit = as.character(unit_val), target = as.character(target_val),
                         class = as.character(prediction_class),
                         value = as.character(value_val), cat = as.character(cat_val), prob = as.character(prob_val),
                         sample = as.character(sample_val), quantile = as.character(quantile_val),
                         family = as.character(family_val), param1 = as.character(param1_val),
                         param2 = as.character(param2_val), param3 = as.character(param3_val))
        # pred_row <- c(unit_val, target_val, prediction_class, value_val, cat_val, prob_val, sample_val,
        #               quantile_val, family_val, param1_val, param2_val, param3_val)
        rows[[length(rows) + 1]] <- as.list(pred_row)  # append
      }
    } else if (prediction_element$class == "named") {
      family_val <- prediction$family
      param1_val <- if (!is.null(prediction$param1)) prediction$param1 else param1_val
      param2_val <- if (!is.null(prediction$param2)) prediction$param2 else param2_val
      param3_val <- if (!is.null(prediction$param3)) prediction$param3 else param3_val
      pred_row <- list(unit = as.character(unit_val), target = as.character(target_val),
                       class = as.character(prediction_class),
                       value = as.character(value_val), cat = as.character(cat_val), prob = as.character(prob_val),
                       sample = as.character(sample_val), quantile = as.character(quantile_val),
                       family = as.character(family_val), param1 = as.character(param1_val),
                       param2 = as.character(param2_val), param3 = as.character(param3_val))
      # pred_row <- c(unit_val, target_val, prediction_class, value_val, cat_val, prob_val, sample_val,
      #               quantile_val, family_val, param1_val, param2_val, param3_val)
      rows[[length(rows) + 1]] <- as.list(pred_row)  # append
    } else if (prediction_element$class == "point") {
      value_val <- prediction$value
      pred_row <- list(unit = as.character(unit_val), target = as.character(target_val),
                       class = as.character(prediction_class),
                       value = as.character(value_val), cat = as.character(cat_val), prob = as.character(prob_val),
                       sample = as.character(sample_val), quantile = as.character(quantile_val),
                       family = as.character(family_val), param1 = as.character(param1_val),
                       param2 = as.character(param2_val), param3 = as.character(param3_val))
      # pred_row <- c(unit_val, target_val, prediction_class, value_val, cat_val, prob_val, sample_val,
      #               quantile_val, family_val, param1_val, param2_val, param3_val)
      rows[[length(rows) + 1]] <- as.list(pred_row)  # append
    } else if (prediction_element$class == "sample") {
      for (sample_idx in seq_along(prediction$sample)) {
        sample_val <- prediction$sample[[sample_idx]]
        pred_row <- list(unit = as.character(unit_val), target = as.character(target_val),
                         class = as.character(prediction_class),
                         value = as.character(value_val), cat = as.character(cat_val), prob = as.character(prob_val),
                         sample = as.character(sample_val), quantile = as.character(quantile_val),
                         family = as.character(family_val), param1 = as.character(param1_val),
                         param2 = as.character(param2_val), param3 = as.character(param3_val))
        # pred_row <- c(unit_val, target_val, prediction_class, value_val, cat_val, prob_val, sample_val,
        #               quantile_val, family_val, param1_val, param2_val, param3_val)
        rows[[length(rows) + 1]] <- as.list(pred_row)  # append
      }
    } else {  # prediction_element$class == "quantile"
      for (quantile_value_idx in seq_along(prediction$quantile)) {
        quantile_val <- prediction$quantile[[quantile_value_idx]]
        value_val <- prediction$value[[quantile_value_idx]]
        pred_row <- list(unit = as.character(unit_val), target = as.character(target_val),
                         class = as.character(prediction_class),
                         value = as.character(value_val), cat = as.character(cat_val), prob = as.character(prob_val),
                         sample = as.character(sample_val), quantile = as.character(quantile_val),
                         family = as.character(family_val), param1 = as.character(param1_val),
                         param2 = as.character(param2_val), param3 = as.character(param3_val))
        # pred_row <- c(unit_val, target_val, prediction_class, value_val, cat_val, prob_val, sample_val,
        #               quantile_val, family_val, param1_val, param2_val, param3_val)
        rows[[length(rows) + 1]] <- as.list(pred_row)  # append
      }
    }
  }
  df <- data.table::rbindlist(rows)
}
