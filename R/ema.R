###############################
# Exponential Moving Averages #
###############################

#' Exponential Moving Average (SMA)
#' 
#' Calculate an exponential moving average (EMA) of a time series.
#' 
#' Four different EMAs types are supported for \code{"uts"} objects. Each type puts different weights on past observation values. The: \itemize{
#' \item \code{equal}: 
#' \item \code{last}: 
#' \item \code{next}: 
#' \item \code{linear}: 
#' }
#' See the reference below for details for precise mathematical definitions and on why one would use one EMA type over another.
#' 
#' @param x a time series object.
#' @param tau a \code{\link[lubridate]{duration}} object, specifying the effective temporal length of the EMA
#' @param type the type of the EMA. Either \code{"equal"}, \code{"last"}, \code{"next"}, or \code{"linear"}. See below for details
#' @param NA_method the method for dealing with \code{NA}s. Either \code{"fail"}, \code{"ignore"}, \code{"omit"}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @references Eckner, A. (2010) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}.
#' @seealso \code{\link{sma}} for simple moving averages.
ema <- function(x, ...) UseMethod("ema")


#' @describeIn ema exponential moving average for \code{"uts"} objects.
#' 
#' @examples
#' #ema(ex_uts(), ddays(1)) 
ema.uts <- function(x, tau, type="last", NA_method="ignore", ...)
{
  # Argument checking and trival cases
  if (!is.duration(tau))
    stop("'tau' is not a duration object")
  if (tau == ddays(0) | (length(x) <= 1))
    return(x)
  
  # Determine first non-NA value of time series
  
}

