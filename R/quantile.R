#' Converts forecast data from Zoltar's native `list` format to a quantile `data.frame`
#'
#' @return A `data.frame` from forecast_data that's the same as [data_frame_from_forecast_data()] does except
#'   only includes point and quantile rows, and with this header: 'location', 'target', 'type', 'quantile',
#'   'value', i.e., 'unit' -> 'location' and 'class' -> 'type'
#' @param forecast_data Forecast data as a `list` in the Zoltar standard format
#' @export
#' @examples \dontrun{
#'   forecast_data <- jsonlite::read_json("docs-predictions.json")
#'   data_frame <- quantile_data_frame_from_forecast_data(forecast_data)
#' }
#' @importFrom rlang .data
quantile_data_frame_from_forecast_data <- function(forecast_data) {
  data_frame_from_forecast_data(forecast_data) %>%
    dplyr::select("unit", "target", "class", "quantile", "value") %>%
    dplyr::rename(location = "unit", type = "class") %>%
    dplyr::filter(.data[["type"]] == "point" | .data[["type"]] == "quantile")
}
