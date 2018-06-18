#####################################
# Helper functions for C interfaces #
#####################################

#' Generic C interface
#' 
#' Generic interface for C-functions with inputs (values, times, length(values), ...) and output (values_new). Example: sma, rolling_max, ema, ...
#' 
#' @param x a numeric \code{"uts"} object with finite, non-NA observation values.
#' @param C_fct the name of the C function to call.
#' @param \dots further arguments passed to the C function.
#' 
#' @keywords internal
#' @examples
#' # SMA_last
#' generic_C_interface(ex_uts(), "sma_last", width_before=ddays(1), width_after=ddays(0))
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
  if (length(x$values) != length(x$times))
    stop("The number of observation values and observation times does not match")
  
  # Call Rcpp wrapper function
  Cpp_fct <- paste0("Rcpp_wrapper_", C_fct)
  values_new <- do.call(Cpp_fct, list(x$values, x$times, ...))
  
  # Generate output time series in efficient way, avoiding calls to POSIXct constructors
  x$values <- values_new
  x
}


