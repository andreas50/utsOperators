##########################
# Simple Moving Averages #
##########################

#' Simple Moving Average (SMA)
#' 
#' Calculate a simple moving average (SMA) of a time series by applying a moving average kernel to the sample path.
#' 
#' Three different time series sample path interpolation schemes are supported for \code{"uts"} objects. Each method implicitly puts different weights on the observation values inside the rolling time window: \itemize{
#'   \item \code{last}: Use \emph{last}-point interpolation for the time series sample path. Equivalently, each observation value is weighted by how long it remained unchanged.
#'   \item \code{next}: Use \emph{next}-point interpolation for the time series sample path. Equivalently, each observation value is weighted by how long it remained the next (i.e. upcoming) observation.
#'   \item \code{linear}: Use \emph{linear} interpolation of the time series sample path. The behavior is approximately halfway in-between last-point and next-point interpolation.
#' }
#' See the first reference below for precise mathematical definitions.
#' 
#' \subsection{Which sample path interpolation method to use?}{
#' Depending on the application, one sample path interpolation method will often be preferable.
#' For example, to calculate the average FED funds target rate over the past three years, it is desirable to weight each observation value by the amount of time it remained unchanged, which is achieved by using method \code{"last"}.
#' On the other hand, method \code{"linear"} can be used to estimate the rolling average value of a discretely-observed continuous-time stochastic processes (see the second reference below for a precise mathematical statement).
#' 
#' However, these SMAs are usually not ideally suited for analyzing discrete events, such as for calculating the average insurance loss per hurricane over the past twelve months, or for determining the average number of IBM common shares traded on the NYSE per executed order during the past 30 minutes.
#' These quantities are \emph{unweighted} averages of the observation values inside a rolling time window, and they can be calculated using \code{rolling_apply} using argument \code{FUN=mean}.
#' }
#' 
#' @param x a numeric time series object.
#' @param width a positive, finite \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#' @param interpolation the sample path interpolation method. Either \code{"last"}, \code{"next"}, or \code{"linear"}. See below for details.
#' @param align either \code{"right"}, \code{"left"}, or \code{"center"}. Specifies the alignment of each output time relative to its corresponding time window. Using \code{"right"} gives a causal (i.e. backward-looking) time series operator, while using \code{"left"} gives a purely forward-looking time series operator.
#' @param interior logical. Should time windows lie entirely in the interior of the temporal support of \code{x}, i.e. inside the time interval \code{[start(x), end(x)]}?
#' @param \dots further arguments passed to or from methods.
#' 
#' @references Eckner, A. (2017) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}.
#' @references Eckner, A. (2017) \emph{Some Properties of Operators for Unevenly Spaced Time Series}.
#' @seealso \code{\link{ema}} for exponential moving averages.
sma <- function(x, ...) UseMethod("sma")


#' @describeIn sma simple moving average for \code{"uts"} objects with finite, non-NA observation values.
#' 
#' @examples
#' sma(ex_uts(), ddays(1))
#' sma(ex_uts(), ddays(1), interpolation="linear")
#' sma(ex_uts(), ddays(1), interpolation="next")
#' 
#' sma(ex_uts(), ddays(1))
#' sma(ex_uts(), ddays(1), align="center")
#' sma(ex_uts(), ddays(1), align="left")
#' 
#' # Plot a monotonically increasing time series 'x' together with
#' # a backward-looking and forward-looking SMA.
#' # Note how the forward-looking SMA is leading the increase in 'x', which
#' # in turn is leading the increase in the backward-looking SMA.
#' \dontrun{
#'   x <- uts(0:10, Sys.time() + dhours(0:10))
#'   par(mfrow=c(1, 3))
#'   plot(x, ylim=c(0, 10), main="Original time series")
#'   plot(sma(x, dhours(3), align="right"), ylim=c(0, 10), main="Backward-looking SMA")
#'   plot(sma(x, dhours(3), align="left"), ylim=c(0, 10), main="Forward-looking SMA")
#' }
#' 
#' # Plot three different SMAs of a monotonically increasing time series.
#' # Note that SMA_last(x)_t <= SMA_linear(x)_t <= SMA_next(x)_t for all observation times t
#' \dontrun{
#'   x <- uts(0:8, Sys.time() + dhours(0:8))
#'   par(mfrow=c(1, 3))
#'   plot(sma(x, dhours(10), interpolation="last"), ylim=c(0, 4), main="Last-point interpolation")
#'   plot(sma(x, dhours(10), interpolation="linear"), ylim=c(0, 4), main="Linear interpolation")
#'   plot(sma(x, dhours(10), interpolation="next"), ylim=c(0, 4), main="Next-point interpolation")
#' }
sma.uts <- function(x, width, interpolation="last", align="right", interior=FALSE, ...)
{
  # Determine the window width before and after the current output time, depending on the window alignment
  check_window_width(width)
  if (align == "right") {
    width_before <- width
    width_after <- 0
  } else if (align == "left") {
    width_before <- 0
    width_after <- width
  } else if (align == "center") {
    width_before <- width / 2
    width_after <- width / 2
  } else
    stop("'align' has to be either 'left', 'right', or 'center")
  
  # Select C function
  if (interpolation == "last")
    C_fct <- "sma_last"
  else if (interpolation == "linear")
    C_fct <- "sma_linear"
  else if (interpolation == "next")
    C_fct <- "sma_next"
  else
    stop("Unknown sample path interpolation method")
  
  # Call C interface for rolling operators
  out <- generic_C_interface(x, width_before=width_before, width_after=width_after, C_fct=C_fct, ...)
  
  # Optionally, drop output times for which the corresponding time window is not completely inside the temporal support of x
  if (interior)
    out <- window(out, start=start(out) + width_before, end(out) - width_after)
  out
}


#' R implementation of sma(..., interpolation="last")
#'
#' This function is identical to \code{\link{sma}} with \code{interpolation="last"}. It exists solely for testing the C implementation.
#'
#' @param x a \code{"uts"} object.
#' @param width a positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#'
#' @keywords internal
#' @examples
#' sma_last_R(ex_uts(), ddays(1)) - sma(ex_uts(), ddays(1), interpolation="last")
sma_last_R <- function(x, width)
{
  # Argument checking
  check_window_width(width)
  if (length(x) <= 1)
    return(x)
  
  # Prepare data for algorithm
  values <- x$values
  if (any(is.na(values)))
    stop("sma does not support NAs yet.")
  times <- as.double(x$times)
  by <- diff(times)
  num_points <- length(values)
  width <- unclass(width)
  
  # Insert artificial observation at time point min(T(X))-width
  times <- c(times[1] - width, times)
  values <- c(values[1], values)
  by <- c(width, by)
  
  # Initialize loop
  left <- 1
  rollsum <- values[1] * width
  out <- numeric(num_points)
  out[1] <- values[1]
  
  # Calculate sma
  for (j in 3:(num_points+1)) {
    # Expand interval on right
    t_new <- times[j]
    t_old <- times[j-1]
    rollsum <- rollsum + values[j-1] * by[j-1]
    
    # Add other half of partly included observation (on left end of interval)
    rollsum <- rollsum + values[left] * ((t_old - width) - times[left])
    
    # Shrink interval of left
    while (times[left+1] <= t_new - width) {
      rollsum <- rollsum - values[left] * by[left]
      left <- left + 1  
    }
    
    # Remove half of partly included observation (on left end of interval)
    rollsum <- rollsum - values[left] * ((t_new - width) - times[left])
    
    # Calculate sma value for current window
    out[j-1] <- rollsum / width
  }
  x$values <- out
  x
}


#' R implementation of sma(..., interpolation="linear")
#'
#' This function is identical to \code{\link{sma}} with \code{interpolation="linear"}. It exists solely for testing the C implementation.
#'
#' @param x a \code{"uts"} object.
#' @param width a positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#'
#' @keywords internal
#' @examples
#' sma_linear_R(ex_uts(), ddays(1)) - sma(ex_uts(), ddays(1), interpolation="linear")
sma_linear_R <- function(x, width)
{
  # Error and trivial case checking
  check_window_width(width)
  if (length(x) <= 1)
    return(x)
  
  # Extract time points an observations times
  values <- x$values
  if (any(is.na(values)))
    stop("sma_lin does not support NAs yet.")
  times <- as.double(x$times)
  by <- diff(times)
  num_points <- length(values)
  width <- unclass(width)
  
  # Insert artificial observation at min(T(X))-width
  times <- c(times[1] - width, times)
  values <- c(values[1], values)
  by <- c(width, by)
  
  # Initialize loop
  left <- 1
  rollsum <- values[1] * width
  out <- numeric(num_points)
  out[1] <- values[1]
  
  # Calculate sma
  for (j in 3:(num_points+1)) {
    # Expand interval on right
    t_left_new <- times[j] - width
    t_left_old <- times[j-1] - width
    rollsum <- rollsum + (values[j-1] + values[j]) * by[j-1] / 2
    
    # Add other half of partly included observation (on left end of interval)
    ll <- t_left_old - times[left]
    hh <- (values[left+1] - values[left]) * ll / (times[left+1] - times[left])
    gamma <- values[left] * ll + hh * ll / 2
    rollsum <- rollsum + gamma
    
    # Shrink interval of left
    while (times[left+1] <= t_left_new) {
      rollsum <- rollsum - (values[left] + values[left+1]) * by[left] / 2
      left <- left + 1  
    }
    
    # Remove half of partly included observation (on left end of interval)
    ll <- t_left_new - times[left]
    hh <- (values[left+1] - values[left]) * ll / (times[left+1] - times[left])
    gamma <- values[left] * ll + hh * ll / 2
    rollsum <- rollsum - gamma
    
    # Calculate sma value for current window
    out[j-1] <- rollsum / width
  }
  x$values <- out
  x
}
