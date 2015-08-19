###########################
# Simple Moving Averages #
##########################

#' Simple Moving Average (SMA)
#' 
#' Calculate a simple moving average (SMA) of a time series.
#' 
#' @param x a \code{"uts"} object with \code{\link{logical}} observation values.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{ema}} for exponential moving averages.
sma <- function(x, ...) UseMethod("sma")


#' @describeIn sma simple moving average for \code{"uts"} objects.
#' @examples
#' sma(ex_uts(), ddays(1))
sma.uts<- function(x, tau, type="last", ...)
{
  # tau   ... a duration object, specifying the length of moving average
  # type  ... moving average type: eq, last, lin, next
  # ...   ... other parameters passed to C functions
  
  if (type == "eq")
    sma_eq(x, tau, ...)
  else if (type == "last")
    sma_last(x, tau, ...)
  else if (type == "lin")
    sma_lin(x, tau, ...)
  else if (type == "next")
    sma_next(x, tau, ...)
  else
    stop("Unknown moving average calculation type")
}