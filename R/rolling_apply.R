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
  list(start_times=start_times, end_times=start_times + by)
}


#' Apply Rolling Function (Static Version)
#' 
#' Apply a function to the time series values in a sequence of user-defined time windows.
#' 
#' @param x a time series object.
#' @param start a strictly increasing \code{\link{POSIXct}}, specifying the start times of the time windows.
#' @param end a strictly increasing \code{\link{POSIXct}} object of same length as \code{start}, and with \code{start[i] <= end[i]} for each \code{1 <= i <= length(start)}. Specifies the end times of the time windows.
#' @param FUN a function to be applied to the vector of observation values in each close time interval \code{[start[i], end[i]]}.
#' @param \dots arguments passed to \code{FUN}.
#' @param align either \code{"right"} (the default), \code{"left"}, or \code{"center"}. Specifies the position of each output time inside the corresponding time window.
#' @param interior logical. Only include time windows \code{[start[i], end[i]]} in the output that are in the interior of the temporal support of \code{x}, i.e. in the interior of the time interval \code{[start(x), end(x)]}.
#' 
#' @keywords internal
#' @seealso \code{\link{rolling_apply}} for a version of this function that \emph{dynamically} determines the time windows.
#' @examples
#' start <- seq(as.POSIXct("2007-11-08"), as.POSIXct("2007-11-09 12:00:00"), by="12 hours")
#' end <- start + dhours(8)
#' rolling_apply_static(ex_uts(), start, end, FUN=mean, interior=TRUE)
#' rolling_apply_static(ex_uts(), start, end, FUN=mean)
#' rolling_apply_static(ex_uts(), start, end, FUN=mean, align="left")
#' rolling_apply_static(ex_uts(), start, end, FUN=mean, align="center")
rolling_apply_static <- function(x, start, end, FUN, ..., align="right", interior=FALSE)
{
  # Argument checking
  if (!is.uts(x))
    stop("'x' is not a UTS object")
  if (!is.POSIXct(start))
    stop("'start' is not a POSIXct object")
  if (!is.POSIXct(end))
    stop("'end' is not a POSIXct object")
  if (any(diff(start) <= 0))
    stop("The window start times (start) need to be a strictly increasing")
  if (any(diff(end) <= 0))
    stop("The window end times (end) need to be a strictly increasing")
  if (length(start) != length(end))
    stop("The number of window start and end times differs")
  if (any(start > end))
    stop("Some of the window end times (end) are before the corresponding start time (start)")
  
  # Remove time windows that are not completely inside the temporal support of x
  if (interior) {
    drop <- (start < start(x)) | (end > end(x))
    start <- start[!drop]
    end <- end[!drop]
  }
  
  # Evaluate function on values in each time window
  FUN <- match.fun(FUN)
  args <- c(list(c()), list(...))
  values_new <- rep(NA_real_, length(start))
  for (j in seq_along(start)) {
    pos <- (x$times >= start[j]) & (x$times <= end[j])
    args[[1]] <- x$values[pos]
    values_new[j] <- do.call(FUN, args)
  }
  
  # Return output time series with proper time alignment
  if (align == "left")
    times_new <- start
  else if (align == "right")
    times_new <- end
  else if (align == "center")
    times_new <- start + (end - start) / 2
  else
    stop("'align' has to be either 'left', 'right', or 'center")
  uts(values_new, times_new)
}


#' Apply Rolling Function
#' 
#' Apply a function to the time series values in a rolling time window.
#' 
#' @param x a time series object.
#' @param width a finite, non-negative \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' @param FUN a function to be applied to the vector of observation values within the rolling time window.
#' @param \dots arguments passed to \code{FUN}.
#' @param by a positive \code{\link[lubridate]{duration}} object. Calculate \code{FUN} on a sequence of time points with this spacing, rather than at every observation time of \code{x}.
#' @param align either \code{"right"} (the default), \code{"left"}, or \code{"center"}. Specifies the alignment each output time to its corresponding time window.
#' @param interior logical. Include only output times where the corresponding time window is entirely in the interior of the temporal support of \code{x,} i.e. in the interior of the time interval \code{[start(x), end(x)]}?
rolling_apply <- function(x, ...) UseMethod("rolling_apply")


#' @describeIn rolling_apply apply rolling function to \code{"uts"} object.
#' 
#' @examples
#' rolling_apply(ex_uts(), width=ddays(0.1), FUN="mean", by=ddays(0.1))
#' rolling_apply(ex_uts(), width=ddays(1), FUN="mean")
#' rolling_apply(ex_uts(), width=ddays(1), FUN="mean", interior=TRUE)
rolling_apply.uts <- function(x, width, FUN, ..., by=NULL, align="right", interior=FALSE)
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
  if (!is.finite(width))
    stop("Only finite window widths (width) are supported at the moment")
  
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
    tmp <- rolling_time_window(start(x) - adj, end(x) - adj, width=width, by=by)
    start_times <- tmp$start_times
    end_times <- tmp$end_times
  }
  
  # Call helper functions that does the remaining work
  rolling_apply_static(x, start=start_times, end=end_times, align=align, interior=interior, FUN=FUN, ...)
}
