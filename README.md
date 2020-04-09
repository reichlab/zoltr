
<!-- README.md is generated from README.Rmd. Please edit that file -->
zoltr - An R client for the Zoltar data repository API
======================================================

Overview
--------

This package contains functions for working with the [Zoltar](https://www.zoltardata.com/) forecast repository's API, including projects, models, forecasts, and scores. Read more about this package at the [zoltr pkgdown site](http://reichlab.io/zoltr/). Documentation on Zolar itself is at [docs.zoltardata.com](https://docs.zoltardata.com/).

Installation
------------

You can install the released version of zoltr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("zoltr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("reichlab/zoltr")
```

Usage
-----

Read more at the [zoltr pkgdown site](http://reichlab.io/zoltr/), but briefly you use the `new_connection()` function to create a connection to [Zoltar](https://www.zoltardata.com/) and then pass that connection along with the *URL* of the resource of interest (e.g., a project, model, or forecast) to this package's various functions like `projects()`, `project_info()`, or `scores()`.

``` r
library(zoltr)
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))
zoltar_connection
#> ZoltarConnection 'https://zoltardata.com' authenticated (exp=2020-04-09 18:00:13 UTC)

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "Docs Example Project", "url"]
the_project_info <- project_info(zoltar_connection, project_url)
names(the_project_info)
#>  [1] "id"                    "url"                   "owner"                
#>  [4] "is_public"             "name"                  "description"          
#>  [7] "home_url"              "logo_url"              "core_data"            
#> [10] "time_interval_type"    "visualization_y_label" "truth"                
#> [13] "model_owners"          "score_data"            "models"               
#> [16] "units"                 "targets"               "timezeros"
the_project_info$name
#> [1] "Docs Example Project"
```

Forecast data format
--------------------

The native forecast data format supported by the Zoltar API is a `list`. See [docs.zoltardata.com](https://docs.zoltardata.com/) for format details. You can find an example at vignettes/docs-predictions.json . By convention this package referred to this as `forecast_data`. This package supports conversion to this format (which is used throughout the package) from the CDC's CSV file format \[1\] via the `forecast_data_from_cdc_csv_file`() function. Future versions will support bidirectional conversion, as well as support for a more general CSV format.

\[1\] Details about the CDC CSV format can be found at [flu\_challenge\_2016-17\_update.docx](https://predict.cdc.gov/api/v1/attachments/flusight/flu_challenge_2016-17_update.docx).
