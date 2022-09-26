#' @keywords internal
"_PACKAGE"


# Suppress R CMD check note
#' @importFrom readr read_csv
#' @importFrom jsonlite read_json
NULL


release_questions <- function() {
  c(
    "Did you update NEWS.md?",
    "Did you update DESCRIPTION Version?",
    "Did you update vignette dates?",
    "Did you re-knit README.Rmd?",
    "Did you run devtools::test()?",
    "Did you run devtools::check()?",
    "Did you run devtools::document()?",
    "Did you run devtools::build_vignettes()?",
    "Did you re-run pkgdown::build_site()?"
  )
}
