#####################################
# Helper functions for C interfaces #
#####################################

#' Generic C interface
#' 
#' Generic interface for C-functions with inputs (values, times, length(values), values_new, ...) and output (values_new). Example: sma, rollingMax, ema, ...
#' 
#' @param x a numeric \code{"uts"} object with finite, non-NA observation values.
#' @param C_fct the name of the C function to call.
#' @param \dots further arguments passed to the C function.
#' 
#' @keywords internal
#' @examples
#' # SMA_last
#' generic_C_interface(ex_uts(), "sma_last", width=ddays(1))
#' 
#' # One- vs. two-sided window
#' generic_C_interface(ex_uts(), "rolling_num_obs", width_before=dhours(6), width_after=dhours(0))
#' generic_C_interface(ex_uts(), "rolling_num_obs", width_before=dhours(6), width_after=dhours(6))
generic_C_interface <- function(x, C_fct, ...)
{
  # Argument checking
  if (!is.uts(x))
    stop("'x' is not a 'uts' object")
  if (!is.numeric(x$values))
    stop("The time series is not numeric")
  if (anyNA(x$values) || any(is.infinite(x$values)))
    stop("The time series observation values have to be finite and not NA")
  
  # Prepare data for C-interface
  values <- as.double(x$values)
  times <- as.double(x$times)
  if (length(values) != length(times))
    stop("The number of observation values and observation times does not match")
  
  # Call C function
  values_new <- .C(C_fct, values=values, times=times, n=as.integer(length(values)),
    values_new=double(length(values)),  ..., NAOK=TRUE)$values_new
  
  # Generate output time series in efficient way, avoiding calls to POSIXct constructors
  x$values <- values_new
  x
}


