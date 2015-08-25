#####################################
# Helper functions for C interfaces #
#####################################

#' Generic C interface
#' 
#' Generic interface for C-functions with inputs (values, times, length(values), values_new, ...) and output (values_new). Example: sma, rollingMax, ema, ...
#' 
#' @param x a \code{"uts"} object.
#' @param C_fct the name of the C function to call.
#' @param NA_method the method for dealing with \code{NA}s. Either \code{"fail"}, \code{"ignore"}, \code{"omit"}.
#' @param \dots further arguments passed to the C function.
#' 
#' @keywords internal
#' @examples
#' # Prepare sample 'uts"
#' x <- ex_uts()
#' x$values[2] <- NA
#' 
#' # SMA_eq
#' generic_C_interface(x, "sma_equal", tau=ddays(1))
#' generic_C_interface(x, "sma_equal", tau=ddays(1), NA_method="omit")
#' 
#' # SMA_last
#' generic_C_interface(x, "sma_last", tau=ddays(1))
#' generic_C_interface(x, "sma_last", tau=ddays(1), NA_method="omit")
generic_C_interface <- function(x, C_fct, NA_method="ignore", ...)
{
  # Argument checking
  if (!is.uts(x))
    stop("'x' is not a 'uts' object")
  if (!is.numeric(x$values))
    stop("The time series is not numeric")
  
  # Trivial case
  if (length(x) == 0)
    return(x)
  
  # Prepare data for C-interface
  values <- as.double(x$values)
  times <- as.double(x$times)
  if (length(values) != length(times))
    stop("The number of observation values and observation times does not match")
  
  # Remove NAs for C function call
  is_na <- is.na(values)
  any_na <- any(is_na)
  if (any_na) {
    na_pos <- which(is.na(values))
    values <- values[-na_pos]
    na_times <- x$times[na_pos]
    times <- times[-na_pos]
  }
  
  # Call C function
  values_new <- .C(C_fct, values=values, times=times, n=as.integer(length(values)),
    values_new=double(length(values)),  ..., NAOK=TRUE)$values_new
  
  # Generate output time series in efficient way, avoiding calls to POSIXct constructors
  out <- x
  out$values <- values_new
  if (any_na) {
    if (NA_method == "ignore") {
      out$times <- x$times[-na_pos]
      out[na_times] <- NA
    } else if (NA_method == "omit")
      out$times <- out$times[-na_pos]
  }
  out
}


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
  x$values <- rev(x$values)
  x$times <- start(x) + (end(x) - rev(x$times))
  x
}


#' Generic C interface for rolling time series operators
#' 
#' This function is a convenience wrapper around \code{\link{generic_C_interface}} that adds argument checking of the rolling window width \code{tau}.
#' 
#' @param x a \code{"uts"} object.
#' @param tau a \code{\link[lubridate]{duration}} object, specifying the temporal length of the rolling time window.
#' @param \dots further arguments passed to \code{\link{generic_C_interface}}.
#' 
#' @keywords internal
#' @examples
#' # Prepare sample 'uts"
#' x <- ex_uts()
#' x$values[2] <- NA
#' 
#' # SMA_eq
#' generic_C_interface_rolling(x, tau=ddays(1), C_fct="sma_equal")
#' generic_C_interface_rolling(x, tau=ddays(1), C_fct="sma_equal", NA_method="omit")
#' 
#' # SMA_last
#' generic_C_interface_rolling(x, tau=ddays(1), C_fct="sma_equal")
#' generic_C_interface_rolling(x, tau=ddays(1), C_fct="sma_equal", NA_method="omit")
generic_C_interface_rolling <- function(x, tau, ...)
{
  # Argument checking
  if (!is.duration(tau))
    stop("The rolling window width 'tau' is not a 'duration' object")
  if (is.na(tau))
    stop("The rolling window width 'tau' cannot be NA")
  if (tau < ddays(0))
    stop("The rolling window width 'tau' cannot be negative")
  
  # Call standardized C-interface
  generic_C_interface(x, tau=tau, ...)
}
