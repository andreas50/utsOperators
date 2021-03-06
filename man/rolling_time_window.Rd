% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rolling_apply.R
\name{rolling_time_window}
\alias{rolling_time_window}
\title{Rolling Time Window}
\usage{
rolling_time_window(start, end, width, by, interior = FALSE)
}
\arguments{
\item{start}{a \code{\link{POSIXct}} object or coercible using \code{\link{as.POSIXct}}. The start time of the first time window.}

\item{end}{a \code{\link{POSIXct}} object or coercible using \code{\link{as.POSIXct}}. The maximum end time of the last time window.}

\item{width}{a non-negative \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.}

\item{by}{a positive \code{\link[lubridate]{duration}} object. The temporal spacing between start times (and therefore also end times) of adjacent time windows.}

\item{interior}{logical. If \code{TRUE}, only include time windows \code{[start_times[i], end_times[i]]} in the output that are in the interior of the temporal support of \code{x}, i.e. in the interior of the time interval \code{[start(x), end(x)]}.}
}
\value{
A list with two \code{POSIXct} objects of equal length, specifying the start and end times of the rolling time window.
}
\description{
Generate a sequence of start times and end times for a rolling time window of specified width.
}
\examples{
rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30))
rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30),
  interior=TRUE)
}
\keyword{internal}
