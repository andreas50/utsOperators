#############################################################################
# Specialized implementations of rolling_apply() for certain choices of FUN #
#############################################################################

#' Apply Rolling Function (Specialized Implementation)
#' 
#' This function provides a fast, specialized implementation of \code{\link{rolling_apply}} for certain choices of \code{FUN} and for \code{by=NULL} (i.e. when moving the rolling time window one observation at a time, rather than by a fixed temporal amount).
#' 
#' It is usually not necessary to call this function, because it is called automatically by \code{\link{rolling_apply}} whenever a specialized implementation is available.
#' 
#' @param x a numeric time series object.
#' @param width a finite, positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' @param FUN a function to be applied to the vector of observation values inside the half-open (open on the left, closed on the right) rolling time window.
#' @param NA_method the method for dealing with \code{NA}s. Either \code{"fail"}, \code{"ignore"}, or \code{"omit"}.
#' @param \ldots further arguments passed to or from methods.
#' 
#' @references Eckner, A. (2010) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}. 
#' @keywords internal
rolling_apply_specialized <- function(x, ...) UseMethod("rolling_apply_specialized")


#' @describeIn rolling_apply_specialized Implementation for \code{"uts"} objects.
#' 
#' @examples
#' # Rolling sum
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=sum)
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=sum) - rolling_apply(ex_uts(), ddays(1), FUN=sum)
#' 
#' # Rolling min/max
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=min)
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=max)
rolling_apply_specialized.uts <- function(x, width, FUN, NA_method="ignore", ...)
{
  # Extract the name of the function to be called
  if (is.function(FUN)) {
    FUN <- deparse(substitute(FUN))
    if (length(FUN) > 1)
      stop("Custom functions (FUN) are not supported")
  }
  
  # Call C function
  if (FUN == "length")
    C_fct <- "rolling_num_obs"
  else if (FUN == "min")
    C_fct <- "rolling_min"
  else if (FUN == "max")
    C_fct <- "rolling_max"
  else if (FUN == "sum")
    C_fct <- "rolling_sum"
  else
    stop("This function does not have a specialized rolling_apply() implementation")
  generic_C_interface_rolling(x, width, C_fct=C_fct, NA_method=NA_method)
}

