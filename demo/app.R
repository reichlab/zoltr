setwd(getSrcDirectory(function(x) {x}))
source("connection.r", chdir = TRUE)


#
# example app
#

# todo:
# - change `source()` to `library(ZoltR)` when made into a package
# - pass args via command line and environment vars
# - put into a main() function, passing args in standard R manner


busy_poll_upload_file_job <- function(upload_file_job) {
    # get the updated status via polling (busy wait every 1 second)
    cat(paste0("- polling for status change. upload_file_job: ", upload_file_job$uri, "\n"))
    while (TRUE) {
        status <- status_as_str(upload_file_job)
        cat(paste0("  = ", status, "\n"))
        if (status == 'FAILED') {
            cat(paste0("  x failed\n"))
            break
        }
        if (status == 'SUCCESS') {
            break
        }
        Sys.sleep(1)
        refresh(upload_file_job)
    }
}


# connect to Zoltar and print all accessible projects
conn <- new_connection(host = "http://127.0.0.1:8000")  # todo: don't pass host
z_authenticate(conn, "model_owner1", "mo1-asdf")
print(conn)

the_projects <- projects(conn)
cat(paste0("* projects (", length(the_projects), ")\n"))
for (project in the_projects) {
    cat(paste0("- (", class(project)[1], ") ", project$uri, ", ", length(project$json), ", ", id(project), ", '",
    name(project), "'\n"))
}

# print a particular project's models
cond <- sapply(the_projects, function(project) name(project) == "public project")
project <- if (any(cond))the_projects[cond][[1]] else NULL
the_models <- models(project)
cat(paste0("* models in " , project$uri, "\n"))
for (model in the_models) {
    cat(paste0("- (", class(model)[1], ") ", model$uri, ", ", length(model$json), ", ", id(model), ", '", name(model),
    "'\n"))
}

# for a particular TimeZero, delete existing Forecast, if any
cond <- sapply(the_models, function(project) name(project) == "Test ForecastModel1")
model <- if (any(cond))the_models[cond][[1]] else NULL
cat(paste0("* working with " , model$uri, "\n"))
cat(paste0("* pre-delete forecasts\n"))
the_forecasts <- forecasts(model)
for (forecast in the_forecasts) {
    cat(paste0("- (", class(forecast)[1], ") ", forecast$uri, ", ", length(forecast$json), ", ", id(forecast), "\n"))
}

the_timezero_date <- "20170117"  # YYYYMMDD_DATE_FORMAT
cond <- sapply(the_forecasts, function(forecast) timezero_date(forecast) == the_timezero_date)
existing_forecast <- if (any(cond))the_forecasts[cond][[1]] else NULL
if (! is.null(existing_forecast)) {
    cat(paste0("- deleting existing forecast", the_timezero_date, ", ", existing_forecast$uri, "\n"))
    delete(existing_forecast)
} else {
    cat(paste0("- no existing forecast: ", the_timezero_date, "\n"))
}

refresh(model)  # o/w model.forecasts errors b/c the just-deleted forecast is still cached in model
cat(paste0("* post-delete forecasts\n"))
the_forecasts <- forecasts(model)
for (forecast in the_forecasts) {
    cat(paste0("- (", class(forecast)[1], ") ", forecast$uri, ", ", length(forecast$json), ", ", id(forecast), "\n"))
}

# upload a new forecast
forecast_csv_file <- "/Users/cornell/IdeaProjects/forecast-repository/forecast_app/tests/EW1-KoTsarima-2017-01-17-small.csv"
upload_file_job <- upload_forecast(model, the_timezero_date, forecast_csv_file)
busy_poll_upload_file_job(upload_file_job)

# get the new forecast from the upload_file_job by parsing the generic 'output_json' field
new_forecast_pk <- upload_file_job$json$output_json$forecast_pk
the_new_forecast <- forecast_for_pk(model, new_forecast_pk)
cat(paste0("* new_forecast: ", the_new_forecast$uri, "\n"))

refresh(model)
cat(paste0("* post-upload forecasts\n"))
the_forecasts <- forecasts(model)
for (forecast in the_forecasts) {
    cat(paste0("- (", class(forecast)[1], ") ", forecast$uri, ", ", length(forecast$json), ", ", id(forecast), "\n"))
}

# get its data
cat(paste0("* data for forecast: ", the_new_forecast$uri, "\n"))

data_json <- data(the_new_forecast, is_json=TRUE)
cat(paste0("- data_json: # metadata: ", length(data_json$metadata), ", # locations: ", length(data_json$locations), "\n"))
# str(data_json)

data_csv <- data(the_new_forecast, is_json=FALSE)
cat(paste0("- data_csv:\n"))
str(data_csv)
