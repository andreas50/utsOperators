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


#' @describeIn rolling_apply_specialized Implementation for \code{"uts"} objects with finite, non-NA observation values.
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
    if (identical(FUN, length))
      FUN <- "length"
    else if (identical(FUN, mean))
      FUN <- "mean"
    else if (identical(FUN, min))
      FUN <- "min"
    else if (identical(FUN, max))
      FUN <- "max"
    else if (identical(FUN, median))
      FUN <- "median"
    else if (identical(FUN, sum))
      FUN <- "sum"
    else {
      FUN <- deparse(substitute(FUN))
      if (length(FUN) > 1)
        stop("Custom functions (FUN) are not supported")
    }
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
  
  # Determine the window width before and after the current output time, depending on the window alignment
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
  
  # Replace NaN by NA in output to be consistent with generic rolling_apply()
  out$values[is.nan(out$values)] <- NA
  
  # Optionally, drop output times for which the corresponding time window is not completely inside the temporal support of x
  if (interior)
    out <- window(out, start=start(out) + width_before, end(out) - width_after)
  out
}


#' Specialized Rolling Apply Available?
#' 
#' Check whether \code{\link{rolling_apply_specialized.uts}} can be called for a given \code{\link{uts}} object with arguments \code{FUN} and \code{by}.
#' 
#' @param x a \code{"uts"} object.
#' @param FUN see \code{\link{rolling_apply_specialized}}.
#' @param by see \code{\link{rolling_apply_specialized}}.
#' 
#' @keywords internal
#' @examples 
#' have_rolling_apply_specialized(ex_uts(), FUN=mean)
#' have_rolling_apply_specialized(ex_uts(), FUN="mean")
#' have_rolling_apply_specialized(ex_uts(), FUN=mean, by=ddays(1))
#' have_rolling_apply_specialized(uts(NA, Sys.time()), FUN=mean)
#' 
#' FUN <- mean
#' have_rolling_apply_specialized(ex_uts(), FUN=FUN)
have_rolling_apply_specialized <- function(x, FUN, by=NULL)
{
  # Extract the name of the function to be called
  if (is.function(FUN)) {
    if (identical(FUN, length))
      FUN <- "length"
    else if (identical(FUN, mean))
      FUN <- "mean"
    else if (identical(FUN, min))
      FUN <- "min"
    else if (identical(FUN, max))
      FUN <- "max"
    else if (identical(FUN, median))
      FUN <- "median"
    else if (identical(FUN, sum))
      FUN <- "sum"
    else
      FUN <- deparse(substitute(FUN))
  }
  
  # Determine if fast special purpose implementation is available
  (length(FUN) == 1) && (FUN %in% c("length", "mean", "min", "max", "median", "sum")) &&
    is.null(by) && (is.numeric(x$values)) && (!anyNA(x$values)) && (all(is.finite(x$values)))
}

