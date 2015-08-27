###########################
# Simple Moving Averages #
##########################

#' Simple Moving Average (SMA)
#' 
#' Calculate a simple moving average (SMA) of a time series.
#' 
#' Four different SMAs types are supported for \code{"uts"} objects. Each type puts different weights on the observation values inside the rolling time window of width \code{tau}: \itemize{
#' \item \code{equal}: Each observation value is weighted equally.
#' \item \code{last}: Apply the moving average kernel to the time series sample path with \emph{last}-point interpolation. Equivalently, each observation value is weighted by how long it remained unchanged.
#' \item \code{next}: Apply the moving average kernel to the time series sample path with \emph{next}-point interpolation. Equivalently, each observation value is weighted by how long it remained the next (i.e. upcomming) observation.
#' \item \code{linear}: Apply the moving average kernel to the time series sample path with \emph{linear} interpolation. The behavior is approximately halfway in-between last-point and next-point interpolation.
#' }
#' See the reference below for precise mathematical definitions and on why one would use one SMA type over another.
#' 
#' @param x a time series object.
#' @param tau a \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' @param type the type of the SMA. Either \code{"equal"}, \code{"last"}, \code{"next"}, or \code{"linear"}. See below for details
#' @param NA_method the method for dealing with \code{NA}s. Either \code{"fail"}, \code{"ignore"}, \code{"omit"}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @references Eckner, A. (2010) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}.
#' @seealso \code{\link{ema}} for exponential moving averages.
sma <- function(x, ...) UseMethod("sma")


#' @describeIn sma simple moving average for \code{"uts"} objects.
#' 
#' @examples
#' sma(ex_uts(), ddays(1))
#' sma(ex_uts(), ddays(1), type="last")
#' sma(ex_uts(), ddays(1), type="linear")
#' sma(ex_uts(), ddays(1), type="next")
#' 
#' # For SMA_equal, the original time series is returned,
#' # if the time window is narrow enough (modulo numerical noise)
#' sma(ex_uts(), dseconds(1), type="equal") - ex_uts()
#' 
#' # Plot a monotonically increasing time series 'x', together with
#' # a backward-looking and forward-looking SMA.
#' # Note that SMA_last(x)_t <= x_t <= SMA_next(x)_t for all observation times t.
#' \dontrun{
#'   x <- uts(1:10, Sys.time() + dhours(1:10))
#'   par(mfrow=c(1, 3))
#'   plot(x)
#'   plot(sma(x, ddays(1)))
#'   plot(sma(x, ddays(-1)))
#' }
sma.uts <- function(x, tau, type="last", NA_method="ignore", ...)
{
  # Argument checking and trival cases
  if (!is.duration(tau))
    stop("'tau' is not a duration object")
  if (tau == ddays(0) | (length(x) <= 1))
    return(x)
  
  # Call generic C interface for rolling operators
  if (type == "equal")
    generic_C_interface_rolling(x, tau, C_fct="sma_equal", NA_method=NA_method, ...)
  else if (type == "last")
    generic_C_interface_rolling(x, tau, C_fct="sma_last", NA_method=NA_method, ...)
  else if (type == "linear")
    generic_C_interface_rolling(x, tau, C_fct="sma_linear", NA_method=NA_method, ...)
  else if (type == "next") {
    # Generate dummy "uts" with observation values shifted backward
    x2 <- uts(values = c(x$values, last(x)),
      times = c(start(x) - days(1), x$times))
    
    # SMA_next = SMA_last on dummy "uts"
    out <- generic_C_interface_rolling(x2, tau, C_fct="sma_last", NA_method=NA_method, ...)
    head(out, -1)
  } else
    stop("Unknown moving average calculation type")
}
