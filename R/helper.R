#' Reverse Observations
#'
#' Reverse the observation values and times inside the time window bounded by the first and last observation time.
#'
#' @param x a \code{"uts"} object.
#'
#' @keywords internal
#' @examples
#' rev(ex_uts())
#' 
#' # Reversing a "uts" reverses the vector of observation values
#' ex_uts()$values
#' rev(ex_uts())$values
#'
#' # Reversing a "uts" reverses the vector of observation time differences
#' diff(ex_uts()$times)
#' diff(rev(ex_uts())$times)
rev.uts <- function(x)
{
  # Remark: as.duration() cast needed to preserve "tzone" attribute
  x$values <- rev(x$values)
  x$times <- start(x) + as.duration((end(x) - rev(x$times)))
  x
}


#' Check Window Width
#' 
#' This helper functions checks if a given window width is valid. It allows to streamline the argument checking inside of \code{\link{generic_C_interface}}, \code{\link{rolling_apply}}, and similar functions.
#' 
#' @return This function does not return a value. It executes successfully if its argument is a valid window width, and stops with an error message otherwise.
#' @param width a non-negative, finite \code{\link[lubridate]{duration}} object, specifying the temporal width of a rolling time window.
#' @param des a description of the argument that is being checked.
#' @param require_positive logical. Whether \code{width} is required to be positive instead of only non-negative.
#' 
#' @keywords internal
#' @examples
#' check_window_width(ddays(1))
check_window_width <- function(width, des="rolling window width", require_positive=TRUE)
{
  if (!is.duration(width))
    stop("The ", des, " is not a 'duration' object")
  if (is.na(width))
    stop("The ", des, " is NA")
  if (!is.finite(width))
    stop("The ", des, " is not finite")
  
  # Optional additional checks
  if (require_positive && (unclass(width) <= 0)) # much faster than S4 method dispatch
    stop("The ", des, " is not positive")
  else if (unclass(width) < 0)
    stop("The ", des, " is negative")
}
