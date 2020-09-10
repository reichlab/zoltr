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
