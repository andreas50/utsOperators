#' Internal Functions
#'
#' The internal functions listed below might be of interest to developers seeking to extend the package functionality.
#' 
#' C interfaces:
#' \itemize{
#'   \item \code{\link{generic_C_interface}}
#'   \item \code{\link{generic_C_interface_rolling}}
#' }
#' 
#' Helper functions:
#' \itemize{
#'   \item \code{\link{rolling_time_window}}
#'   \item \code{\link{rolling_time_window_indices}}
#' }
#' 
#' \code{uts} methods:
#' \itemize{
#'   \item \code{\link[=rev.uts]{rev}}
#'   \item \code{\link[=rolling_apply_static]{rolling_apply_static}}
#'   \item \code{\link{sma_equal_R}}
#'   \item \code{\link{sma_last_R}}
#'   \item \code{\link{sma_linear_R}}
#' }
#' 
#' Specialized \code{\link{rolling_apply}} implementations for certain functions:
#' \itemize{
#'   %\item \code{\link{rolling_max.uts}} (\code{FUN = max})
#'   %\item \code{\link{rolling_min.uts}} (\code{FUN = min})
#'   %\item \code{\link{rolling_num_obs.uts}} (\code{FUN = length})
#'   \item \code{\link{rolling_sum.uts}} (\code{FUN = sum})
#' }
#' 
#' @name utsOperators-internal
NULL