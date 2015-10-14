###############################
# Exponential Moving Averages #
###############################

#' Exponential Moving Average (EMA)
#' 
#' Calculate an exponential moving average (EMA) of a time series.
#' 
#' Four different EMAs types are supported for \code{"uts"} objects. Each type puts different weights on past observation values: \itemize{
#' \item \code{last}: Apply the exponential moving average kernel to the time series sample path with \emph{last}-point interpolation. Equivalently, each observation value is weighted proportionally by how long it remained unchanged.
#' \item \code{next}: Apply the exponential moving average kernel to the time series sample path with \emph{next}-point interpolation. Equivalently, each observation value is weighted proportionally by how long it remained the next (i.e. upcomming) observation. For equally spaced time series this method coincides with the usual definition used for such time series, see Proposition 8.8 in Eckner, A. (2014).
#' \item \code{linear}: Apply the exponential moving average kernel to the time series sample path with \emph{linear} interpolation. The behavior is approximately halfway in-between last-point and next-point interpolation.
#' }
#' See the first reference below for precise mathematical definitions.
#' 
#' \subsection{Which EMA \code{type} to use?}{
#' Depending on the application, one interpolation type will often be preferable. See the corresponding discussion for \code{\link[=sma]{simple moving averages}}.
#' }
#' 
#' @param x a time series object.
#' @param tau a finite \code{\link[lubridate]{duration}} object, specifying the effective temporal length of the EMA. Use positive values for backward-looking (i.e. normal, causal) EMAs, and negative values for forward-looking EMAs.
#' @param type the type of the EMA. Either \code{"last"}, \code{"next"}, or \code{"linear"}. See below for details.
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
#' ema(ex_uts(), ddays(1), type="linear")
#' ema(ex_uts(), ddays(1), type="next")
#' 
#' # Plot a monotonically increasing time series 'x', together with
#' # a backward-looking and forward-looking EMA.
#' # Note how the forward-looking SMA is leading the increase in 'x', which
#' # in turn is leading the increase in the backward-looking SMA.
#' \dontrun{
#'   x <- uts(0:10, Sys.time() + dhours(0:10))
#'   par(mfrow=c(1, 3))
#'   plot(x, ylim=c(0, 10), main="Original time series")
#'   plot(ema(x, dhours(3)), ylim=c(0, 10), main="Backward-looking EMA")
#'   plot(ema(x, dhours(-3)), ylim=c(0, 10), main="Forward-looking EMA")
#' }
#' 
#' # Plot three different EMAs of a monotonically increasing time series
#' # Note that EMA_last(x)_t <= EMA_linear(x)_t <= EMA_next(x)_t for all observation times t
#' \dontrun{
#'   x <- uts(0:8, Sys.time() + dhours(0:8))
#'   par(mfrow=c(1, 3))
#'   plot(ema(x, dhours(10), type="last"), ylim=c(0, 3), main="Last-point interpolation")
#'   plot(ema(x, dhours(10), type="linear"), ylim=c(0, 3), main="Linear interpolation")
#'   plot(ema(x, dhours(10), type="next"), ylim=c(0, 3), main="Next-point interpolation")
#' }
ema.uts <- function(x, tau, type="last", NA_method="ignore", ...)
{
  # Argument checking and special case (not handled by C code)
  if (!is.duration(tau))
    stop("'tau' is not a duration object")
  if (unclass(tau) == 0)
    return(x)

  # For forward-looking EMAs, call an appropriate EMA on the time-reversed time series
  if (unclass(tau) < 0) {
    # Need to switch types "next" and "last"
    x_rev <- rev(x)
    if (type == "next")
      type_rev <- "last"
    else if (type == "last")
      type_rev <- "next"
    else
      type_rev <- type
    
    # Call C interface and reverse output again
    tmp <- ema(x_rev, tau=abs(tau), type=type_rev, NA_method=NA_method, ...)
    return(rev(tmp))
  }
  
  # Call generic C interface for rolling operators
  if (type == "next")
    generic_C_interface_rolling(x, tau, C_fct="ema_next", NA_method=NA_method, ...)
  else if (type == "last")
    generic_C_interface_rolling(x, tau, C_fct="ema_last", NA_method=NA_method, ...)
  else if (type == "linear")
    generic_C_interface_rolling(x, tau, C_fct="ema_linear", NA_method=NA_method, ...)
  else
    stop("Unknown EMA calculation type")
}
