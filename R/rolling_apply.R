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

