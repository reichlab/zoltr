# zoltr-temp

## implementation notes
this file combines two approaches to writing a library:
- using environments (not the more traditional lists) for class instances
  = https://rstudio-pubs-static.s3.amazonaws.com/150296_904158e070594471864e89c10c0d14f9.html
- using generic functions vs. OO-style functions definined within objects
  = see [talked w/nick re: library usage style - oo vs. generic functions]


## todo
- tests!
- think accessors that just go directly to json - id(), name(), etc. instead more general 'attribute' or similar?
- think generic functions that don't need to be, i.e., that won't be exported: json_for_uri(), get_token()
- session optional (public resources don't require)
- httr::POST
- export

