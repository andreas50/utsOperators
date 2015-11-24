###############################
# Exponential Moving Averages #
###############################

#' Exponential Moving Average (EMA)
#' 
#' Calculate an exponential moving average (EMA) of a time series by applying an exponential kernel to the time series sample path.
#' 
#'Three different time series sample path interpolation schemes are supported for \code{"uts"} objects. Each method implicitly puts different weights on past observation values: \itemize{
#'   \item \code{last}: Use \emph{last}-point interpolation for the time series sample path.. Equivalently, each observation value is weighted proportionally by how long it remained unchanged.
#'   \item \code{next}: Use \emph{next}-point interpolation for the time series sample path. Equivalently, each observation value is weighted proportionally by how long it remained the next (i.e. upcoming) observation. For equally spaced time series this method coincides with the usual definition used for such time series, see Proposition 8.8 in Eckner, A. (2014).
#'   \item \code{linear}: Use \emph{linear} interpolation of the time series sample path. The behavior is approximately halfway in-between last-point and next-point interpolation.
#' }
#' See the first reference below for precise mathematical definitions.
#' 
#' \subsection{Which sample path interpolation method to use?}{
#' Depending on the application, one sample path interpolation method will often be preferable. See the corresponding discussion for \code{\link[=sma]{simple moving averages}}.
#' }
#' 
#' @param x a numeric time series object.
#' @param tau a finite \code{\link[lubridate]{duration}} object, specifying the effective temporal length of the EMA. Use positive values for backward-looking (i.e. normal, causal) EMAs, and negative values for forward-looking EMAs.
#' @param interpolation the sample path interpolation method. Either \code{"last"}, \code{"next"}, or \code{"linear"}. See below for details.
#' @param \dots further arguments passed to or from methods.
#' 
#' @references Eckner, A. (2010) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}.
#' @references Eckner, A. (2014) \emph{Some Properties of Operators for Unevenly Spaced Time Series}.
#' @seealso \code{\link{sma}} for simple moving averages.
ema <- function(x, ...) UseMethod("ema")


#' @describeIn ema exponential moving average for \code{"uts"} objects with finite, non-NA observation values.
#' 
#' @examples
#' ema(ex_uts(), ddays(1))
#' ema(ex_uts(), ddays(1), interpolation="linear")
#' ema(ex_uts(), ddays(1), interpolation="next")
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
#'   plot(ema(x, dhours(10), interpolation="last"), ylim=c(0, 3), main="Last-point interpolation")
#'   plot(ema(x, dhours(10), interpolation="linear"), ylim=c(0, 3), main="Linear interpolation")
#'   plot(ema(x, dhours(10), interpolation="next"), ylim=c(0, 3), main="Next-point interpolation")
#' }
ema.uts <- function(x, tau, interpolation="last", ...)
{
  # Argument checking and special case (not handled by C code)
  if (!is.duration(tau))
    stop("'tau' is not a duration object")
  if (unclass(tau) == 0)  # much faster than S4 method dispatch
    return(x)

  # For forward-looking EMAs, call an appropriate EMA on the time-reversed time series
  if (unclass(tau) < 0) { # much faster than S4 method dispatch
    # Need to switch interpolation method "next" and "last"
    x_rev <- rev(x)
    if (interpolation == "next")
      interpolation_rev <- "last"
    else if (interpolation == "last")
      interpolation_rev <- "next"
    else
      interpolation_rev <- interpolation
    
    # Call C interface and reverse output again
    tmp <- ema(x_rev, tau=abs(tau), interpolation=interpolation_rev, ...)
    return(rev(tmp))
  }
  
  # Call generic C interface for rolling operators
  check_window_width(tau, des="EMA half-life")
  if (interpolation == "next")
    generic_C_interface(x, tau, C_fct="ema_next", ...)
  else if (interpolation == "last")
    generic_C_interface(x, tau, C_fct="ema_last", ...)
  else if (interpolation == "linear")
    generic_C_interface(x, tau, C_fct="ema_linear", ...)
  else
    stop("Unknown sample path interpolation method")
}
