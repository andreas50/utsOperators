#####################################
# Helper functions for C interfaces #
#####################################

#' Generic C interface
#' 
#' Generic interface for C-functions with inputs (values, times, length(values), values_new, ...) and output (values_new). Example: sma, rollingMax, ema, ...
#' 
#' @param x a numeric \code{"uts"} object.
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
#' # SMA_last
#' generic_C_interface(x, "sma_last", width=ddays(1))
#' generic_C_interface(x, "sma_last", width=ddays(1), NA_method="omit")
#' 
#' generic_C_interface(ex_uts(), "rolling_num_obs", width_before=dhours(6), width_after=dhours(0))
#' generic_C_interface(ex_uts(), "rolling_num_obs", width_before=dhours(6), width_after=dhours(6))
generic_C_interface <- function(x, C_fct, NA_method="ignore", ...)
{
  # Argument checking
  if (!is.uts(x))
    stop("'x' is not a 'uts' object")
  if (!is.numeric(x$values))
    stop("The time series is not numeric")
  
  # Prepare data for C-interface
  values <- as.double(x$values)
  times <- as.double(x$times)
  if (length(values) != length(times))
    stop("The number of observation values and observation times does not match")
  
  # Remove NAs for C function call
  is_na <- is.na(values)
  any_na <- any(is_na)
  if (any_na) {
    if (NA_method == "fail")
      stop("One or more observation values are NA")
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


