% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project.R
\name{create_timezero}
\alias{create_timezero}
\title{Create a timezero}
\usage{
create_timezero(
  zoltar_connection,
  project_url,
  timezero_date,
  data_version_date = NULL,
  is_season_start = FALSE,
  season_name = ""
)
}
\arguments{
\item{zoltar_connection}{A \code{ZoltarConnection} object as returned by \code{\link[=new_connection]{new_connection()}}}

\item{project_url}{url of a project in zoltar_connection's projects. this is the project the new timezero will be
created in}

\item{timezero_date}{The timezero's date in YYYY-MM-DD format as documented at https://docs.zoltardata.com/fileformats/#project-creation-configuration-json}

\item{data_version_date}{Optional data version date in the same format. Pass NULL if the timezero does not have one}

\item{is_season_start}{TRUE if this starts a season, and FALSE otherwise}

\item{season_name}{Applicable when is_season_start is true, names the season, e.g., "2010-2011"}
}
\value{
model_url of the newly-created timezero
}
\description{
Creates the timezero in the passed project using the passed list. Fails if a timezero with the passed timezero_date
already exists.
}
\examples{
\dontrun{
  new_timezero_url <- create_timezero(conn, "https://www.zoltardata.com/api/project/9/",
                     "2022-11-08", "2022-11-09", TRUE, "2010-2011")
}
}
