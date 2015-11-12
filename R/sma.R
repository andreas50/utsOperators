##########################
# Simple Moving Averages #
##########################

#' Simple Moving Average (SMA)
#' 
#' Calculate a simple moving average (SMA) of a time series.
#' 
#' Four different SMAs types are supported for \code{"uts"} objects. Each type puts different weights on the observation values inside the rolling time window of width \code{width}: \itemize{
#'   \item \code{equal}: Each observation value is weighted equally.
#'   \item \code{last}: Apply the moving average kernel to the time series sample path with \emph{last}-point interpolation. Equivalently, each observation value is weighted by how long it remained unchanged.
#'   \item \code{next}: Apply the moving average kernel to the time series sample path with \emph{next}-point interpolation. Equivalently, each observation value is weighted by how long it remained the next (i.e. upcomming) observation.
#'   \item \code{linear}: Apply the moving average kernel to the time series sample path with \emph{linear} interpolation. The behavior is approximately halfway in-between last-point and next-point interpolation.
#' }
#' See the first reference below for precise mathematical definitions.
#' 
#' \subsection{Which SMA \code{type} to use?}{
#' Type \code{"equal"} is ideal for analyzing discrete events; for example, to calculate the average insurance loss per hurricane over the past twelve months, or to determine the average number of IBM common shares traded on the NYSE per executed order during the past 30 minutes.
#' 
#' The other three types can be used to analyze periodic measurements on the same object. Depending on the application, one interpolation type will often be preferable.
#' For example, to calculate the average FED funds target rate over the past three years, it is desirable to weight each observation value by the amount of time it remained unchanged, which is achieved by using type \code{"last"}.
#' On the other hand, type \code{"linear"} can be used to estimate the rolling average value of a discretely-observed continuous-time stochastic processes (see the second reference below for a precise mathematical statement).
#' }
#' 
#' @param x a numeric time series object.
#' @param width a positive, finite \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window. Use positive values for backward-looking (i.e. normal, causal) SMAs, and negative values for forward-looking SMAs.
#' @param type the type of the SMA. Either \code{"equal"}, \code{"last"}, \code{"next"}, or \code{"linear"}. See below for details.
#' @param NA_method the method for dealing with \code{NA}s. Either \code{"fail"}, \code{"ignore"}, or \code{"omit"}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @references Eckner, A. (2010) \emph{Algorithms for Unevenly Spaced Time Series: Moving Averages and Other Rolling Operators}.
#' @references Eckner, A. (2014) \emph{Some Properties of Operators for Unevenly Spaced Time Series}.
#' @seealso \code{\link{ema}} for exponential moving averages.
sma <- function(x, ...) UseMethod("sma")


#' @describeIn sma simple moving average for \code{"uts"} objects.
#' 
#' @examples
#' sma(ex_uts(), ddays(1))
#' sma(ex_uts(), ddays(1), type="equal")
#' sma(ex_uts(), ddays(1), type="linear")
#' sma(ex_uts(), ddays(1), type="next")
#' 
#' # For SMA_equal, the original time series is returned,
#' # if the time window is narrow enough (modulo numerical noise)
#' sma(ex_uts(), dseconds(1), type="equal") - ex_uts()
#' 
#' # Plot a monotonically increasing time series 'x' together with
#' # a backward-looking and forward-looking SMA.
#' # Note how the forward-looking SMA is leading the increase in 'x', which
#' # in turn is leading the increase in the backward-looking SMA.
#' \dontrun{
#'   x <- uts(0:10, Sys.time() + dhours(0:10))
#'   par(mfrow=c(1, 3))
#'   plot(x, ylim=c(0, 10), main="Original time series")
#'   plot(sma(x, dhours(3)), ylim=c(0, 10), main="Backward-looking SMA")
#'   plot(sma(x, dhours(-3)), ylim=c(0, 10), main="Forward-looking SMA")
#' }
#' 
#' # Plot three different SMAs of a monotonically increasing time series
#' # Note that SMA_last(x)_t <= SMA_linear(x)_t <= SMA_next(x)_t for all observation times t
#' \dontrun{
#'   x <- uts(0:8, Sys.time() + dhours(0:8))
#'   par(mfrow=c(1, 3))
#'   plot(sma(x, dhours(10), type="last"), ylim=c(0, 4), main="Last-point interpolation")
#'   plot(sma(x, dhours(10), type="linear"), ylim=c(0, 4), main="Linear interpolation")
#'   plot(sma(x, dhours(10), type="next"), ylim=c(0, 4), main="Next-point interpolation")
#' }
sma.uts <- function(x, width, type="last", NA_method="ignore", ...)
{
  # For forward-looking SMAs, call an appropriate SMA on the time-reversed time series
  if (unclass(width) < 0) { # much faster than S4 method dispatch
    # Need to switch types "next" and "last"
    x_rev <- rev(x)
    if (type == "next")
      type_rev <- "last"
    else if (type == "last")
      type_rev <- "next"
    else
      type_rev <- type
    
    # Call C interface and reverse output again
    tmp <- sma(x_rev, width=abs(width), type=type_rev, NA_method=NA_method, ...)
    return(rev(tmp))
  }
  
  # Call generic C interface for rolling operators
  if (type == "equal")
    generic_C_interface_rolling(x, width, C_fct="sma_equal", NA_method=NA_method, ...)
  else if (type == "last")
    generic_C_interface_rolling(x, width, C_fct="sma_last", NA_method=NA_method, ...)
  else if (type == "linear")
    generic_C_interface_rolling(x, width, C_fct="sma_linear", NA_method=NA_method, ...)
  else if (type == "next")
    generic_C_interface_rolling(x, width, C_fct="sma_next", NA_method=NA_method, ...)
  else
    stop("Unknown moving average calculation type")
}


#' R implementation of sma(..., type="equal")
#'
#' This function is identical to \code{\link{sma}}  with \code{type="equal"}, except that the \code{NA_method} argument is not supported. It exists solely for testing the C implementation.
#'
#' This function exists solely for testing the C implementation.
#'
#' @param x a \code{"uts"} object.
#' @param width a positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#'
#' @keywords internal
#' @examples
#' sma_equal_R(ex_uts(), ddays(1)) - sma(ex_uts(), ddays(1), type="equal")
sma_equal_R <- function(x, width)
{
  # Argument checking and special cases
  if (!is.duration(width))
    stop("The length/width of the rolling operator is not a 'duration' object")
  if (is.na(width))
    stop("The length/width of the rolling window is equal to NA")
  if (unclass(width) <= 0) # much faster than S4 method dispatch
    stop("The length/width of the rolling operator is not positive")
  if ((length(x) <= 1) | (unclass(width) == 0)) # much faster than S4 method dispatch
    return(x)
  
  # Prepare data for algorithm
  values <- x$values
  n <- length(values)
  values_new <- numeric(n)
  times <- as.double(x$times)
  width <- unclass(width)
  
  # Calculate moving average
  for (j in 1:n) {
    used <- (times > times[j] - width) & (times <= times[j])  # use half-open interval
    values_new[j] <- mean(values[used])
  }
  uts(values_new, x$times)
}


#' R implementation of sma(..., type="last")
#'
#' This function is identical to \code{\link{sma}} with \code{type="last"}, except that the \code{NA_method} argument is not supported. It exists solely for testing the C implementation.
#'
#' @param x a \code{"uts"} object.
#' @param width a positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#'
#' @keywords internal
#' @examples
#' sma_last_R(ex_uts(), ddays(1)) - sma(ex_uts(), ddays(1), type="last")
sma_last_R <- function(x, width)
{
  # Argument checking
  if (!is.duration(width))
    stop("The length/width of the rolling operator is not a 'duration' object")
  if (is.na(width))
    stop("The length/width of the rolling window is equal to NA")
  if (unclass(width) <= 0) # much faster than S4 method dispatch
    stop("The length/width of the rolling operator is not positive")
  if (length(x) <= 1 | unclass(width) == 0) # much faster than S4 method dispatch
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


#' R implementation of sma(..., type="linear")
#'
#' This function is identical to \code{\link{sma}} with \code{type="linear"}, except that the \code{NA_method} argument is not supported. It exists solely for testing the C implementation.
#'
#' @param x a \code{"uts"} object.
#' @param width a positive \code{\link[lubridate]{duration}} object, specifying the temporal width of the rolling time window.
#'
#' @keywords internal
#' @examples
#' sma_linear_R(ex_uts(), ddays(1)) - sma(ex_uts(), ddays(1), type="linear")
sma_linear_R <- function(x, width)
{
  # Error and trivial case checking
  if (!is.duration(width))
    stop("The length/width of the rolling operator is not a 'duration' object")
  if (is.na(width))
    stop("The length/width of the rolling window is equal to NA")
  if (unclass(width) <= 0) # much faster than S4 method dispatch
    stop("The length/width of the rolling operator is not positive")
  if (length(x) <= 1 | unclass(width) == 0) # much faster than S4 method dispatch
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
