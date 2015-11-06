#############################################################################
# Specialized implementations of rolling_apply() for certain choices of FUN #
#############################################################################

#' Apply Rolling Function (Specialized Implementation)
#' 
#' This function provides a fast, specialized implementation of \code{\link{rolling_apply}} for certain choices of \code{FUN} and for \code{by=NULL} (i.e. when moving the rolling time window one observation at a time, rather than by a fixed temporal amount).
#' 
#' It is usually not necessary to call this function. Instead, it is called automatically by \code{\link{rolling_apply}} whenever a specialized implementation is available.
#' 
#' @param x a numeric time series object.
#' @param width a finite, positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' 
#' @references Eckner, A. (2010) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}. 
#' @keywords internal
rolling_apply_specialized <- function(x, ...) UseMethod("rolling_apply_specialized")


#' Rolling Average/Length/Minimun/Maximum/Sum
#' 
#' @examples
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=sum)
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN="sum")
#' 
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=sum) - rolling_apply(ex_uts(), ddays(1), FUN=sum)
rolling_apply_specialized.uts <- function(x, width, FUN)
{
  # Argument checking
  if (!is.duration(width))
    stop("'width' is not a duration object")
  if (as.numeric(width) <= 0)
    stop("'width' is not positive")
  if (!is.finite(width))
    stop("Only finite window widths (width) are supported at the moment")
  
  # Extract the name of the function to be called
  if (is.function(FUN)) {
    FUN <- deparse(substitute(FUN))
    if (length(FUN) > 1)
      stop("Custom functions (FUN) are not supported")
  }
  
  # Call C function
  if (FUN == "sum")
    generic_C_interface_rolling(x, width, C_fct="rolling_sum")
  else
    stop("This function does not have a specialized rolling_apply() implementation")
}

