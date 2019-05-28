#' @keywords internal
"_PACKAGE"


# Suppress R CMD check note
#' @importFrom readr read_csv
#' @importFrom jsonlite read_json
#' @importFrom mockery stub
#' @importFrom webmockr stub_request
NULL


release_questions <- function() {
  c(
    "Did you update NEWS.Rmd?",
    "Did you update DESCRIPTION Version?",
    "Did you update vignette dates?",
    "Did you re-knit README.Rmd?",
    "Did you run devtools::check()?",
    "Did you run devtools::document()?",
    "Did you run devtools::build_vignettes()?",
    "Did you re-run pkgdown::build_site()?"
  )
}
