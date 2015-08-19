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
#' generic_C_interface(x, "sma_eq", tau=ddays(1))
#' generic_C_interface(x, "sma_eq", tau=ddays(1), NA_method="omit")
#' 
#' # SMA_last
#' generic_C_interface(x, "sma_last", tau=ddays(1))
#' generic_C_interface(x, "sma_last", tau=ddays(1), NA_method="omit")
generic_C_interface <- function(x, C_fct, NA_method="ignore", ...)
{
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


#' Generic C interface for rolling time series operators
#' 
#' Generic interface for rolling operators with one parameter tau (window width), and zero or more other parameters
#' 
#' @param tau a \code{\link[lubridate]{duration}} object, specifying the temporal length of the rolling time window.
#' @param \dots further arguments passed to \code{\link{generic_C_interface}}.
#' 
#' @keywords internal
generic_C_interface_rolling <- function(x, tau, NA_method="reinsert", C_fct, ...)
{
  # Argument checking
  if (missing(C_fct) | missing(tau))
    stop("One or more arguments missing.")
  if (!is.duration(tau))
    stop("The length/width of the rolling operator needs is not a 'duration' object.")
  if (is.na(tau))
    stop("Length of rolling window is equal to NA")
  if (tau < ddays(0))
    stop("Length of rolling window is negative.")
  
  # Trivial cases
  if (length(x) == 0)
    return(x)
  
  # Call standardized C-interface
  generic_C_interface(x, tau=as.double(tau), ...)
}
