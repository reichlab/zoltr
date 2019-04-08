# zoltr-temp

This package provides an R interface for working with the (https://www.zoltardata.com/)[Zoltar] forecast repository's
API.


## todo
- add UploadFileJob getter for: upload_file_job$json$output_json$forecast_pk
- document data() return formats - JSON, CSV
- add function documentation - http://r-pkgs.had.co.nz/man.html#man-functions
- think getters that just go directly to json - id(), name(), etc. E.g., get_attribute(zoltar_resource, attr_name).
  then include a table of all defined classes and their attributes
- make `authenticate()` optional (public resources don't require authentication)
- make into actual package
- change app to a vignette - http://r-pkgs.had.co.nz/misc.html


## User notes
To use the package, create a new connection to Zoltar using `new_connection()` and then pass that connection to the
package's generic functions to access resources like _Projects_, _Models_, and _Forecasts_. See the example app for
more.

- Most functions return objects corresponding to Zoltar resources, which make it easier for you to work with. The entry
  point for getting objects is the `projects()` generic function, which is passed the _ZoltarConnection_ that
  `new_connection()` returns.
- Access to protected resources and actions (like uploading a file or getting a private project's models) requires
  authenticating using the `z_authenticate()` function. Pass it the username and password for your account. Note that
  currently Zoltar uses a five minute timeout on the underlying JWT tokens used under the hood, which means you'll have
  to re-authenticate after that time. Later some kind of auto-re-authenticate might be written.
- Keep in mind that Zoltar uses an asychronous messaging queue for long operations like `upload_forecast()`, which makes
  the Zoltar API a little more complicated. Rather than a function blocking, you get a response that's a _UploadFileJob_
  which you can poll to get its current status.


## implementation notes
This file combines two approaches to writing a library:

- using environments (not the more traditional lists) for class instances
  = https://rstudio-pubs-static.s3.amazonaws.com/150296_904158e070594471864e89c10c0d14f9.html
- using generic functions vs. OO-style functions definined within objects

