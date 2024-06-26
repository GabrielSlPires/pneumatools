% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PneumatronData_summary.R
\name{summary,PneumatronDatabase-method}
\alias{summary,PneumatronDatabase-method}
\title{Summary method for PneumatronDatabase}
\usage{
\S4method{summary}{PneumatronDatabase}(object)
}
\arguments{
\item{object}{A PneumatronDatabase object.}
}
\description{
Provides a summary of the PneumatronDatabase object, including the number of measurements,
start and end times for each Pneumatron device, and identifies measurements that need
to be checked or removed.
}
\examples{
\dontrun{
database <- open_pneumatron_database("path/to/your/datafile.csv", data_format = "V4")
summary(database)
}
}
