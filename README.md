
<!-- README.md is generated from README.Rmd. Please edit that file -->
zoltr - An R client for the Zoltar data repository API
======================================================

<!-- badges: start -->
<!-- badges: end -->
This package contains functions for working with the [https://www.zoltardata.com/](Zoltar) forecast repository's API, including projects, models, forecasts, and scores. Read more at the [zoltr pkgdown site](http://reichlab.io/zoltr/).

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

Read more at the [zoltr pkgdown site](http://reichlab.io/zoltr/), but briefly you use the `new_connection()` function to create a connection to [https://www.zoltardata.com/](Zoltar) and then pass that connection along with the *ID* of the resource of interest (e.g., a project, model, or forecast) to this package's various functions like `projects()`, `project_info()`, or `scores()`.

``` r
library(zoltr)
conn <- new_connection()
conn
#> ZoltarConnection 'http://zoltardata.com' (no session)

the_projects <- projects(conn)
project_id <- the_projects[1,]$id
the_project_info <- project_info(conn, project_id)
names(the_project_info)
#>  [1] "id"           "url"          "owner"        "is_public"   
#>  [5] "name"         "description"  "home_url"     "core_data"   
#>  [9] "config_dict"  "template"     "truth"        "model_owners"
#> [13] "score_data"   "models"       "locations"    "targets"     
#> [17] "timezeros"
the_project_info$name
#> [1] "CDC Flu challenge"
```
