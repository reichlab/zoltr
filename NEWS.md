# zoltr 1.0.1
- added support for [create new prediction_types for mean and median #367](https://github.com/reichlab/forecast-repository/issues/367)

# zoltr 1.0.0
- added support for [add capacity to specify the issued_at date for an uploaded truth data batch #50](https://github.com/reichlab/zoltr/issues/50) by adding optional `issued_at` argument to `upload_truth()`

# zoltr 0.9.11
- fixed bug introduced in [Add support of uploading in zoltar CSV format #345](https://github.com/reichlab/forecast-repository/issues/345)


# zoltr 0.9.10
- fixed [False error message: non-decreasing values as quantiles increase #49](https://github.com/reichlab/zoltr/issues/49)


# zoltr 0.99
- added support for [Add support of uploading in zoltar CSV format #345](https://github.com/reichlab/forecast-repository/issues/345) by changing the `upload_forecast()` function to accept a new `is_json` argument, making the new signature:  `upload_forecast(zoltar_connection, model_url, timezero_date, forecast_data, is_json = TRUE, notes = "")`


# zoltr 0.98
- added `create_timezero <- function(zoltar_connection, project_url, timezero_date, data_version_date, is_season_start, season_name)` function


# zoltr 0.97
- changed `job_info(zoltar_connection, job_url)` to support [add missing timezero, unit, and target info to truth upload's job.output_json #344](https://github.com/reichlab/forecast-repository/issues/344) - by parsing `job_json$output_json$missing_time_zeros` into date objects.


# zoltr 0.96
- added `upload_truth(zoltar_connection, project_url, truth_csv_file)` function


# zoltr 0.95
- added support for [prototype Covid-19-Hub-Vizualization integration #329](https://github.com/reichlab/forecast-repository/issues/329): removed visualization_y_label and time_interval_type fields.


# zoltr 0.941
- cleaned up dependencies. thanks to Hugo Gruson


# zoltr 0.94
- added support for [change Target fields to support visualization #328](https://github.com/reichlab/forecast-repository/issues/328): adjusted Zoltar's Target data model to add `outcome_variable` to all targets, and to add `numeric_horizon` and `reference_date_type` fields to `is_step_ahead` ones. These changes support upcoming visualization features.


# zoltr 0.93
- added support for [expand Unit to include human-readable field(s) #228](https://github.com/reichlab/forecast-repository/issues/228)


# zoltr 0.92
- added support for `as_of` in truth queries


# zoltr 0.91
- replaced `issue_date` date field with `issued_at` datetime field to match server change 


# zoltr 0.9
- added `latest_forecasts(zoltar_connection, project_url)` function


# zoltr 0.9
- added `latest_forecasts(zoltar_connection, project_url)` function


# zoltr 0.8
- updated package to be compatible with Zoltar 4


# zoltr 0.7
- removed scoring-related features, which are transitioning from the server to client libraries


# zoltr 0.6.6
- changed `truth_info()` to match a server change in truth naming: now has `source` and `created_at` instead  of
  `truth_csv_filename` and `truth_updated_at`, respectively


# zoltr 0.6.5
- changed truth retrieval from file-based to query-based (like forecast and score data)


# zoltr 0.6.4
- changed `forecast_info()` and `forecasts()` to support forecast issue_date


# zoltr 0.6.3
- changed `job_data()` (and therefore `do_zoltar_query()`) to parse the quantile column as double (was defaulting to
  logical in some cases)


# zoltr 0.6.2
- `models()`: added "notes" column
- `forecasts()`: added three timezero columns in addition to "url": "timezero_date", "data_version_date",
   "is_season_start"


# zoltr 0.6.1
- added error message tweaks, fixed small example typos


# zoltr 0.6.0
- (API BREAK) Made two major changes. 1) Changed score downloading from the `scores()` function (removed from the
  library) that downloaded all scores to querying scores in a manner very similar to that of forecasts. This
  entailed generalizing `do_zoltar_query`, `submit_query()`, and `job_data()`, which now take a `is_forecast_query` arg.
  The first two now accept either `"types"` or `"scores"`, depending on that arg. 2) Changed those two score query
  functions to pass strings, not database IDs, removing the need to call `query_with_ids()`, which has been deleted.


# zoltr 0.5.10
- Changed `do_zoltar_query()` query args to all be optional.


# zoltr 0.5.9
- Fixed `job_data()` to return `units` column as character, not numeric.
- Added `units` arg to `do_zoltar_query()` to match `submit_query()`.


# zoltr 0.5.8
- Changed `busy_poll_job()` to check for the new TIMEOUT status.


# zoltr 0.5.7
- Changed `query_with_ids()` to warn and ignore invalid query values.


# zoltr 0.5.6
- Changed `query_with_ids()`'s `query` arg to accept model abbreviations in "models", instead of passing them in
  "model_abbrs", which is no longer accepted. This is because Zoltar now uses model abbreviations to uniquely identify
  them, not names.


# zoltr 0.5.5
- Added convenience functions: `do_zoltar_query()` and `busy_poll_job()`.


# zoltr 0.5.4
- Added support for additional model metadata.


# zoltr 0.5.3
- Added support for querying project forecasts.


# zoltr 0.5.2
- Added functions to convert forecast data to `data.frames`s.


# zoltr 0.5.1
- Updated package to be compatible with Zoltar 3.1.0.


# zoltr 0.5.0
- Updated entire package to be compatible with Zoltar 3.


# zoltr# zoltr 0.2.2
- Added automatic re-authentication of expired tokens


# zoltr 0.2.1
- Made changes to avoid CRAN NOTE:

    checking dependencies in R code ... NOTE
    Namespaces in Imports field not imported from:
    ‘jsonlite’ ‘mockery’ ‘readr’ ‘webmockr’
    All declared Imports should be used.


# zoltr 0.2.0
- Initial private beta release
