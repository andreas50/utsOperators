##################################################################
# Apply a function to the time series values in a rolling window #
##################################################################

#' Rolling Time Window
#' 
#' Generate a sequence of start times and end times for a rolling time window of specified width.
#' 
#' @return A list with two \code{POSIXct} objects of equal length, specifying the start and end times of the rolling time window.
#' @param start a \code{\link{POSIXct}} object or coercible using \code{\link{as.POSIXct}}. The start time of the first time window.
#' @param end a \code{\link{POSIXct}} object or coercible using \code{\link{as.POSIXct}}. The maximum end time of the last time window.
#' @param width a non-negative \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' @param by a positive \code{\link[lubridate]{duration}} object. The temporal spacing between start times (and therefore also end times) of adjacent time windows.
#' 
#' @keywords internal
#' @examples
#' rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30))
rolling_time_window <- function(start, end, width, by)
{
  # Argument checking
  if (!is.duration(width))
    stop("'width' is not a duration object")
  if (as.numeric(width) < 0)
    stop("'width' is negative")
  if (!is.duration(by))
    stop("'by' is not a duration object")
  if (as.numeric(by) <= 0)
    stop("'by' is not positive")
  if (!is.POSIXct(start))
    start <- as.POSIXct(start)
  if (!is.POSIXct(end))
    end <- as.POSIXct(end)
  if (start > end)
    stop("'start' cannot be after 'end'")
  
  # Determine the window start and end times
  start_times <- seq(start, end - by, by=by)
  list(start_times=start_times, end_time=start_times + by)
}


#' Apply Rolling Function
#' 
#' Apply a function to the time series values in a rolling time window.
#' 
#' @param x a time series object.
#' @param width a non-negative \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' @param FUN a function to be applied to the vector of observation values within the rolling time window.
#' @param \dots arguments passed to \code{FUN}.
#' @param by a positive \code{\link[lubridate]{duration}} object. Calculate \code{FUN} on a sequence of time points with this spacing, rather than at every observation time of \code{x}.
#' @param align 
rolling_apply <- function(x, ...) UseMethod("rolling_apply")


#' @describeIn rolling_apply apply rolling function to \code{"uts"} object.
#' 
#' @examples
#' rolling_apply(ex_uts(), width=ddays(0.1), FUN="mean", by=ddays(0.1))
#' rolling_apply(ex_uts(), width=ddays(1), FUN="mean")
rolling_apply.uts <- function(x, width, FUN, ..., by=NULL, align="right")
{
  # Argument checking
  if (!is.duration(width))
    stop("'width' is not a duration object")
  if (as.numeric(width) < 0)
    stop("'width' is negative")
  if (!is.null(by)) {
    if (!is.duration(by))
      stop("'by' is not a duration object")
    if (as.numeric(by) <= 0)
      stop("'by' is not positive")
  }
  
  # Determine the rolling time window
  if (is.null(by)) {
    start_times <- window(x, end=end(x) - width)$times
    end_times <- start_times + width
  } else {
    tmp <- rolling_time_window(start(x), end(x), width=width, by=by)
    start_times <- tmp$start_times
    end_times <- tmp$end_times
  }
  
  # Determine set of values in each subinterval
  if (0) {
    #subperiod_values <- rolling_window_values_optimized(x, rolling_window)
  } else {
    subperiod_values <- list()
    for (j in 1:length(start_times))
      subperiod_values[[j]] <- window(x, start_times[j], end_times[j])$values
  }
  non_empty <- which(sapply(subperiod_values, length) > 0)
  
  # Evaluate function on values in each time-window of interest
  FUN <- match.fun(FUN)
  args <- c(list(c()), list(...))
  values_new <- rep(NA, length(start_times))
  for (j in non_empty) {  # slow because of loop, but fast for large values of 'by'
    window_values <- subperiod_values[[j]]
    args[[1]] <- window_values
    values_new[j] <- do.call(FUN, args)
  }
  
  # Return output time series with proper time alignment
  new_ticks <- start_times  # needs updating
  uts(values_new, new_ticks)
}