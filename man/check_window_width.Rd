% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{check_window_width}
\alias{check_window_width}
\title{Check Window Width}
\usage{
check_window_width(width, des = "rolling window width",
  require_positive = TRUE)
}
\arguments{
\item{width}{a non-negative, finite \code{\link[lubridate]{duration}} object, specifying the temporal width of a rolling time window.}

\item{des}{a description of the argument that is being checked.}

\item{require_positive}{logical. Whether \code{width} is required to be positive instead of only non-negative.}
}
\value{
This function does not return a value. It executes successfully if its argument is a valid window width, and stops with an error message otherwise.
}
\description{
This helper functions checks if a given window width is valid. It allows to streamline the argument checking inside of \code{\link{generic_C_interface}}, \code{\link{rolling_apply}}, and similar functions.
}
\examples{
check_window_width(ddays(1))
}
\keyword{internal}
