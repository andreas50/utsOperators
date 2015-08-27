###############################
# Exponential Moving Averages #
###############################

#' Exponential Moving Average (SMA)
#' 
#' Calculate an exponential moving average (EMA) of a time series.
#' 
#' Four different EMAs types are supported for \code{"uts"} objects. Each type puts different weights on past observation values: \itemize{
#' \item \code{last}: Apply the exponential moving average kernel to the time series sample path with \emph{last}-point interpolation. Equivalently, each observation value is weighted proportionally by how long it remained unchanged.
#' \item \code{next}: Apply the exponential moving average kernel to the time series sample path with \emph{next}-point interpolation. Equivalently, each observation value is weighted proportionally by how long it remained the next (i.e. upcomming) observation. For equally spaced time series this method coincides with the usual definition used for such time series, see Proposition 8.8 in Eckner, A. (2014).
#' \item \code{linear}: Apply the exponential moving average kernel to the time series sample path with \emph{linear} interpolation. The behavior is approximately halfway in-between last-point and next-point interpolation.
#' }
#' See the reference below for details for precise mathematical definitions and on why one would use one EMA type over another.
#' 
#' @param x a time series object.
#' @param tau a \code{\link[lubridate]{duration}} object, specifying the effective temporal length of the EMA
#' @param type the type of the EMA. Either \code{"equal"}, \code{"last"}, \code{"next"}, or \code{"linear"}. See below for details.
#' @param NA_method the method for dealing with \code{NA}s. Either \code{"fail"}, \code{"ignore"}, \code{"omit"}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @references Eckner, A. (2010) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}.
#' @references Eckner, A. (2014) \emph{Some Properties of Operators for Unevenly Spaced Time Series}.
#' @seealso \code{\link{sma}} for simple moving averages.
ema <- function(x, ...) UseMethod("ema")


#' @describeIn ema exponential moving average for \code{"uts"} objects.
#' 
#' @examples
#' ema(ex_uts(), ddays(1))
#' ema(ex_uts(), ddays(1), type="equal")
#' ema(ex_uts(), ddays(1), type="linear")
#' ema(ex_uts(), ddays(1), type="next")
#' 
#' # Plot a monotonically increasing time series 'x', together with
#' # a backward-looking and forward-looking EMA.
#' # Note that EMA_last(x)_t <= x_t <= EMA_next(x)_t for all observation times t.
#' \dontrun{
#'   x <- uts(1:10, Sys.time() + dhours(1:10))
#'   par(mfrow=c(1, 3))
#'   plot(x)
#'   plot(ema(x, ddays(1)))
#'   plot(ema(x, ddays(-1)))
#' }
ema.uts <- function(x, tau, type="last", NA_method="ignore", ...)
{
  # Argument checking and trival cases
  if (!is.duration(tau))
    stop("'tau' is not a duration object")
  if (tau == ddays(0) | (length(x) <= 1))
    return(x)

  # Call generic C interface for rolling operators
  if ((type == "equal") | (type == "next"))
    generic_C_interface_rolling(x, tau, C_fct="ema_equal", NA_method=NA_method, ...)
  else if (type == "last")
    generic_C_interface_rolling(x, tau, C_fct="ema_last", NA_method=NA_method, ...)
  else if (type == "linear")
    generic_C_interface_rolling(x, tau, C_fct="ema_linear", NA_method=NA_method, ...)
  else
    stop("Unknown EMA calculation type")
}
