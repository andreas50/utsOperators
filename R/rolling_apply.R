##################################################################
# Apply a function to the time series values in a rolling window #
##################################################################

#' Rolling Time Window
#' 
#' Generate a sequence of start times and end times for a rolling time window of specified width.
#' 
#' @return A list with two \code{POSIXct} objects of equal length, specifying the start and end times of the rolling time window.
#' @param start a \code{\link{POSIXct}} object or coercible using \code{\link{as.POSIXct}}. The alignment point of the first time window.
#' @param end a \code{\link{POSIXct}} object or coercible using \code{\link{as.POSIXct}}. The latest possible alignment point.
#' @param width a non-negative \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' @param by a positive \code{\link[lubridate]{duration}} object. The temporal spacing between alignment points (and therefore also start times and end times) of adjacent time windows.
#' @param align either \code{"right"} (the default), \code{"left"}, or \code{"center"}. Specifies how the rolling windows should be aligned relative to the sequence of time points generated between \code{start} and \code{end}.
#' 
#' @keywords internal
#' @examples
#' rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30))
rolling_time_window <- function(start, end, width, by, align="right")
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
  
  # Determine the window start points
  start_times <- seq(start, end, by=by)
  if (align == "right")
    start_times <- start_times - by
  else if (align == "left") {
    # nothing to do
  } else if (align == "center")
    start_times <- start_times - by / 2
  else
    stop("'align' has to be either 'right', 'left', or 'center'")
  
  # Determine window start points, return window start- and end-points as POSIXct_VECTOR
  list(start_times=start_times, end_time=start_times + by)
}


