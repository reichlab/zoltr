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
