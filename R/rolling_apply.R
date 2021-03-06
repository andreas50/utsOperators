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
#' @param interior logical. If \code{TRUE}, only include time windows \code{[start_times[i], end_times[i]]} in the output that are in the interior of the temporal support of \code{x}, i.e. in the interior of the time interval \code{[start(x), end(x)]}.
#' 
#' @keywords internal
#' @examples
#' rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30))
#' rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30),
#'   interior=TRUE)
rolling_time_window <- function(start, end, width, by, interior=FALSE)
{
  # Argument checking
  check_window_width(width, require_positive=FALSE)
  if (!is.duration(by))
    stop("'by' is not a duration object")
  if (unclass(by) <= 0) # much faster than S4 method dispatch
    stop("'by' is not positive")
  if (!is.POSIXct(start))
    start <- as.POSIXct(start)
  if (!is.POSIXct(end))
    end <- as.POSIXct(end)
  if (start > end)
    stop("'start' cannot be after 'end'")
  
  # Determine the window start and end times
  if (interior)
    start_times <- seq(start, end - by, by=by)
  else
    start_times <- seq(start, end, by=by)
  list(start_times=start_times, end_times=start_times + width)
}


#' Rolling Time Window Indices
#' 
#' For a sorted sequence of time points, determine the start and end indices inside a half-open (open on the left, closed on the right) rolling time window.
#' 
#' @return A list with two integer vectors of equal length, specifying the start and end index in \code{times} of each rolling time window. If the start index is larger than the end index, that means that no observation lies in the corresponding time window.
#' @param times a \code{\link{POSIXct}} object of strictly increasing time points.
#' @param start_times a strictly increasing \code{\link{POSIXct}} object, specifying the start times of the time windows.
#' @param end_times a strictly increasing \code{\link{POSIXct}} object of same length as \code{start}, and with \code{start[i] <= end[i]} for each \code{1 <= i <= length(start)}. Specifies the end times of the time windows.
#' 
#' @keywords internal
#' @examples
#' tmp <- rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30))
#' times <- seq(as.POSIXct("2014-12-01"), as.POSIXct("2015-12-30"), by="week")
#' rolling_time_window_indices(times, tmp$start_times, tmp$end_times)
rolling_time_window_indices <- function(times, start_times, end_times)
{
  # Argument checking
  if (!is.POSIXct(times))
    stop("'times' is not a POSIXct object")
  if (!is.POSIXct(start_times))
    stop("'start_times' is not a POSIXct object")
  if (!is.POSIXct(end_times))
    stop("'end_times' is not a POSIXct object")
  if (is.unsorted(start_times, strictly=TRUE))
    stop("The window start times (start_times) need to be a strictly increasing")
  if (is.unsorted(end_times, strictly=TRUE))
    stop("The window end times (end_times) need to be a strictly increasing")
  if (length(start_times) != length(end_times))
    stop("The number of window start and end times differs")
  if (any(start_times > end_times))
    stop("Some of the window end times are before the corresponding start time")
  
  # Determine start and end indices
  end_index <- num_leq_sorted(end_times, times)  
  #start_index <- pmin(num_less_sorted(start_times, times) + 1, length(times))
  start_index <- num_leq_sorted(start_times, times) + 1L
  
  # Return indices as list
  list(start_index=start_index, end_index=end_index)
}


#' Apply Rolling Function (Static Version)
#' 
#' Apply a function to the time series values in a sequence of user-defined, half-open time windows.
#' 
#' @param x a numeric time series object.
#' @param start_times a \code{\link{POSIXct}} object of strictly increasing time points, specifying the start times of the time windows.
#' @param end_times a \code{\link{POSIXct}} object of strictly increasing time points, of same length as \code{start_times}, and with \code{start_times[i] <= end_times[i]} for each \code{1 <= i <= length(start_times)}. Specifies the end times of the time windows.
#' @param FUN a function to be applied to the vector of observation values inside each half-open time interval \code{(start_times[i], end_times[i]]}.
#' @param \dots arguments passed to \code{FUN}.
#' @param align either \code{"right"} (the default), \code{"left"}, or \code{"center"}. Specifies the position of each output time inside the corresponding time window.
#' @param interior logical. If \code{TRUE}, only include time windows \code{[start_times[i], end_times[i]]} in the output that are in the interior of the temporal support of \code{x}, i.e. in the interior of the time interval \code{[start(x), end(x)]}.
#' 
#' @keywords internal
#' @seealso \code{\link{rolling_apply}} for a version of this function that \emph{dynamically} determines the time windows.
#' @examples
#' start_times <- seq(as.POSIXct("2007-11-08"), as.POSIXct("2007-11-09 12:00:00"), by="12 hours")
#' end_times <- start_times + dhours(8)
#' rolling_apply_static(ex_uts(), start_times, end_times, FUN=mean, interior=TRUE)
#' rolling_apply_static(ex_uts(), start_times, end_times, FUN=mean)
#' rolling_apply_static(ex_uts(), start_times, end_times, FUN=mean, align="left")
#' rolling_apply_static(ex_uts(), start_times, end_times, FUN=mean, align="center")
rolling_apply_static <- function(x, start_times, end_times, FUN, ..., align="right", interior=FALSE)
{
  # Argument checking
  if (!is.uts(x))
    stop("'x' is not a 'uts' object")
  if (!is.numeric(x$values))
    stop("The time series is not numeric")
  if (!is.POSIXct(start_times))
    stop("'start_times' is not a POSIXct object")
  if (!is.POSIXct(end_times))
    stop("'end_times' is not a POSIXct object")
  if (is.unsorted(start_times, strictly=TRUE))
    stop("The window start times (start_times) need to be a strictly increasing")
  if (is.unsorted(end_times, strictly=TRUE))
    stop("The window end times (end_times) need to be a strictly increasing")
  if (length(start_times) != length(end_times))
    stop("The number of window start and end times differs")
  if (any(start_times > end_times))
    stop("Some of the window end times are before the corresponding start time")
  
  # Remove time windows that are not completely inside the temporal support of x
  if (interior) {
    drop <- (start_times < start(x)) | (end_times > end(x))
    start_times <- start_times[!drop]
    end_times <- end_times[!drop]
  }
  
  # Determine observation indices for each time window
  window_indices <- rolling_time_window_indices(x$times, start_times, end_times)
  start_index <- window_indices$start_index
  end_index <- window_indices$end_index
  
  # Evaluate function on values in each time window
  FUN <- match.fun(FUN)
  values <- x$values    # attach to avoid constant dereferencing
  helper <- function(start, end) {
    if (end >= start)
      FUN(values[start:end], ...)
    else
      FUN(values[c()], ...)
  }
  # Equivalent to loop, easier to read (only slightly faster though)
  values_new <- as.numeric(mapply(helper, start_index, end_index))
  
  # Return output time series with proper time alignment
  if (align == "left")
    times_new <- start_times
  else if (align == "right")
    times_new <- end_times
  else if (align == "center")
    times_new <- start_times + (end_times - start_times) / 2
  else
    stop("'align' has to be either 'left', 'right', or 'center")
  uts(values_new, times_new)
}


#' Apply Rolling Function
#' 
#' Apply a function to the time series values in a half-open (open on the left, closed on the right) rolling time window of fixed temporal width.
#' 
#' A fast optimized implementation is used automatically for certain choices of \code{FUN}. See the \code{use_specialized} argument for details.
#' 
#' @param x a numeric time series object.
#' @param width a finite, positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' @param FUN a function to be applied to the vector of observation values inside the half-open rolling time window.
#' @param \dots arguments passed to \code{FUN}.
#' @param by a positive \code{\link[lubridate]{duration}} object. If not \code{NULL}, move the rolling time window by steps of this size forward in time, rather than by the observation time differences of \code{x}.
#' @param align either \code{"right"}, \code{"left"}, or \code{"center"}. Specifies whether the output times should right- or left-aligned or centered compared to their time window. Using \code{"right"} gives a causal (i.e. backward-looking) time series operator, while using \code{"left"} gives a purely forward-looking time series operator.
#' @param interior logical. If \code{TRUE}, then \code{FUN} is only applied if the corresponding time window is in the interior of the temporal support of \code{x}, i.e. inside the time interval \code{[start(x), end(x)]}.
#' @param use_specialized logical. Whether to use a fast optimized implementation, if available. Currently, the following choices for \code{FUN} are supported: \code{mean}, \code{median}, \code{min}, \code{max}, \code{prod}, \code{sd}, \code{sum}, \code{var}
rolling_apply <- function(x, ...) UseMethod("rolling_apply")


#' @describeIn rolling_apply apply rolling function to \code{"uts"} object.
#' 
#' @examples
#' # rolling mean, sum, number of observations
#' rolling_apply(ex_uts(), width=ddays(1), FUN=mean)
#' rolling_apply(ex_uts(), width=ddays(1), FUN=sum)
#' rolling_apply(ex_uts(), width=ddays(1), FUN=length)
#' 
#' # move rolling time window by one observation vs. one day at a time
#' rolling_apply(ex_uts(), width=ddays(1), FUN="mean")
#' rolling_apply(ex_uts(), width=ddays(1), FUN="mean", by=ddays(1))
#' 
#' # right-align, left-aligned, and centered rolling time window
#' rolling_apply(ex_uts(), width=ddays(1), FUN=mean)
#' rolling_apply(ex_uts(), width=ddays(1), FUN=mean, align="left")
#' rolling_apply(ex_uts(), width=ddays(1), FUN=mean, align="center")
#' 
#' # restrict rolling time window to temporal support of x
#' rolling_apply(ex_uts(), width=ddays(1), FUN="mean", interior=TRUE)
#' 
#' # specialized vs. general-purpose implementation
#' rolling_apply(ex_uts(), width=ddays(1), FUN="mean")
#' rolling_apply(ex_uts(), width=ddays(1), FUN="mean", use_specialized=FALSE)    # same
rolling_apply.uts <- function(x, width, FUN, ..., by=NULL, align="right", interior=FALSE, use_specialized=TRUE)
{
  # Call fast special purpose implementation, if available
  if (use_specialized && have_rolling_apply_specialized(x, FUN=FUN, by=by))
    return(rolling_apply_specialized(x, width=width, FUN=FUN, align=align, interior=interior))
  
  # Argument checking
  check_window_width(width)
  if (!is.null(by)) {
    if (!is.duration(by))
      stop("'by' is not a duration object")
    if (unclass(by) <= 0) # much faster than S4 method dispatch
      stop("'by' is not positive")
  }

  # For each time window, determine the output time adjustment relative to 'start'
  if (align == "left")
    adj <- ddays(0)
  else if (align == "right")
    adj <- width
  else if (align == "center")
    adj <- width / 2
  else
    stop("'align' has to be either 'left', 'right', or 'center")
  
  # Determine the rolling time window
  if (is.null(by)) {
      start_times <- x$times - adj
      end_times <- start_times + width
  } else {
    tmp <- rolling_time_window(start(x) - adj, end(x) - adj, width=width, by=by, interior=interior)
    start_times <- tmp$start_times
    end_times <- tmp$end_times
  }
  
  # Call helper functions that does the remaining work
  rolling_apply_static(x, start_times, end_times, align=align, interior=interior, FUN=FUN, ...)
}

