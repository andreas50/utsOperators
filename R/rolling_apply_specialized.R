#############################################################################
# Specialized implementations of rolling_apply() for certain choices of FUN #
#############################################################################

#' Apply Rolling Function (Specialized Implementation)
#' 
#' This function provides a fast, specialized implementation of \code{\link{rolling_apply}} for certain choices of \code{FUN} and for \code{by=NULL} (i.e. when moving the rolling time window one observation at a time, rather than by a fixed temporal amount).
#' 
#' It is usually not necessary to call this function, because it is called automatically by \code{\link{rolling_apply}} whenever a specialized implementation is available.
#' 
#' @param x a numeric time series object with finite, non-NA observation values.
#' @param width a finite, positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' @param FUN a function to be applied to the vector of observation values inside the half-open (open on the left, closed on the right) rolling time window.
#' @param align either \code{"right"}, \code{"left"}, or \code{"center"}. Specifies the alignment of each output time relative to its corresponding time window. Using \code{"right"} gives a causal (i.e. backward-looking) time series operator, while using \code{"left"} gives a purely forward-looking time series operator.
#' @param interior logical. Should time windows lie entirely in the interior of the temporal support of \code{x}, i.e. inside the time interval \code{[start(x), end(x)]}?
#' @param \ldots further arguments passed to or from methods.
#' 
#' @references Eckner, A. (2010) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}. 
#' @keywords internal
rolling_apply_specialized <- function(x, ...) UseMethod("rolling_apply_specialized")


#' @describeIn rolling_apply_specialized Implementation for \code{"uts"} objects.
#' 
#' @examples
#' rolling_apply_specialized(ex_uts(), dhours(12), FUN=length)
#' rolling_apply_specialized(ex_uts(), dhours(12), FUN=length, align="center")
#' rolling_apply_specialized(ex_uts(), dhours(12), FUN=length, align="left")
#' 
#' rolling_apply_specialized(ex_uts(), dhours(12), FUN=length)
#' rolling_apply_specialized(ex_uts(), dhours(12), FUN=length, interior=TRUE)
#' 
#' # Rolling sum
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=sum)
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=sum) - rolling_apply(ex_uts(), ddays(1), FUN=sum)
#' 
#' # Rolling min/max
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=min)
#' rolling_apply_specialized(ex_uts(), ddays(1), FUN=max)
rolling_apply_specialized.uts <- function(x, width, FUN, align="right", interior=FALSE, ...)
{
  # Extract the name of the function to be called
  if (is.function(FUN)) {
    FUN <- deparse(substitute(FUN))
    if (length(FUN) > 1)
      stop("Custom functions (FUN) are not supported")
  }
  
  # Select C function
  if (FUN == "length")
    C_fct <- "rolling_num_obs"
  else if (FUN == "min")
    C_fct <- "rolling_min"
  else if (FUN == "max")
    C_fct <- "rolling_max"
  else if (FUN == "mean")
    C_fct <- "rolling_mean"
  else if (FUN == "median")
    C_fct <- "rolling_median"
  else if (FUN == "sum")
    C_fct <- "rolling_sum"
  else
    stop("This function does not have a specialized rolling_apply() implementation")
  
  # Determine the window width before and after the current output time, depending on the chosen alignment
  check_window_width(width)
  if (align == "right") {
    width_before <- width
    width_after <- 0
  } else if (align == "left") {
    width_before <- 0
    width_after <- width
  } else if (align == "center") {
    width_before <- width / 2
    width_after <- width / 2
  } else
    stop("'align' has to be either 'left', 'right', or 'center")
    
  
  # Call C function 
  out <- generic_C_interface(x, width_before=width_before, width_after=width_after, C_fct=C_fct)
  
  # Optionally, drop output times for which the corresponding time window is not completely inside the temporal support of x
  if (interior)
    out <- window(out, start=start(out) + width_before, end(out) - width_after)
  out
}

