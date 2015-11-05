#############################################################################
# Specialized implementations of rolling_apply() for certain choices of FUN #
#############################################################################

#' Rolling Sum
#' 
#' Calculate the rolling sum of all observation values in a rolling time window. This method is a fast, specialized implementation of \code{\link{rolling_apply}} with \code{FUN=sum} and \code{by=NULL} (i.e. move the rolling time window one observation at a time, rather than by a fixed temporal amount).
#' 
#' @param x a numeric time series object.
#' @param width a finite, positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' 
#' @references Eckner, A. (2010) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}. 
#' @seealso \code{\link{rolling_apply}}
#' @keywords internal
#' 
#' @examples
#' rolling_sum.uts(ex_uts(), ddays(1))
#' rolling_sum.uts(ex_uts(), ddays(1)) - rolling_apply(ex_uts(), ddays(1), FUN=sum)
rolling_sum.uts <- function(x, width)
{
  # Argument checking
  if (!is.duration(width))
    stop("'width' is not a duration object")
  if (as.numeric(width) <= 0)
    stop("'width' is not positive")
  if (!is.finite(width))
    stop("Only finite window widths (width) are supported at the moment")
  
  # Call C function
  generic_C_interface_rolling(x, width, C_fct="rolling_sum")
}
