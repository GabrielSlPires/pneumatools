% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateGasDischarged.R
\name{initial_pressure}
\alias{initial_pressure}
\title{Get initial pressure for gas discharge calculation}
\usage{
initial_pressure(log_line, pressure)
}
\arguments{
\item{log_line}{Vector of integers representing the log line of measured pressures.}

\item{pressure}{Vector of measured pressures from the Pneumatron in kPa.}
}
\value{
Integer position of the lowest pressure in the sequence.
}
\description{
Returns the position of the lowest pressure, which is the initial pressure, necessary for gas discharge calculations.
This function is used internally and is not exported for end users.
}
\details{
This function finds the minimum pressure point before a specified threshold in the log line sequence to define the initial pressure point for calculations.
}
