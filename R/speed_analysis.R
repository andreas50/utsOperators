#############################################
# Speed analysis of R vs. C implementations #
#############################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3

### sma(..., type="equal")
# -) for a moderate-length time series, the C implementation is around 500 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  tau <- ddays(100)
  
  # R vs. C: 0.01s vs. 2.20s
  system.time(for (j in 1:50) sma(ts1, tau, type="equal"))
  system.time(for (j in 1:50) sma_equal_R(ts1, tau))
  
  # Profile C implementation
  # -) ~20% of time spent in C implementation
  # -) argument checking takes up most of the time
  Rprof(interval=0.01)
  for (j in 1:2e4) sma(ts1, tau, type="equal")
  Rprof(NULL)
  summaryRprof()
}


### sma(..., type="last")
# -) for a moderate-length time series, the C implementation is around 120 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  tau <- ddays(100)
  
  # R vs. C: 0.01s vs. 2.06s
  system.time(for (j in 1:200) sma(ts1, tau, type="last"))
  system.time(for (j in 1:200) sma_last_R(ts1, tau))
  
  # Profile C implementation
  # -) ~15% of time spent in C implementation
  # -) argument checking takes up most of the time
  Rprof(interval=0.01)
  for (j in 1:2e4) sma(ts1, tau, type="last")
  Rprof(NULL)
  summaryRprof()
}


### sma(..., type="linear")
# -) for a moderate-length time series, the C implementation is around 200 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  tau <- ddays(100)
  
  # R vs. C: 0.01s vs. 3.55s
  system.time(for (j in 1:200) sma(ts1, tau, type="linear"))
  system.time(for (j in 1:200) sma_linear_R(ts1, tau))
  
  # Profile C implementation
  # -) ~20% of time spent in C implementation
  # -) argument checking takes up most of the time
  Rprof(interval=0.01)
  for (j in 1:2e4) sma(ts1, tau, type="linear")
  Rprof(NULL)
  summaryRprof()
}


### rolling_apply
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  by <- ddays(50)
  
  # Move window one observation at a time: 1.23s
  system.time(for (j in 1:100) rolling_apply(ts1, width=width, FUN="mean"))
  
  # Move window in big steps: 0.62s
  system.time(for (j in 1:200) rolling_apply(ts1, width=width, FUN="mean", by=by))
  
  # Profile implementation (move one observation at a time)
  # -) almost all time spent on argument checking
  Rprof(interval=0.01)
  for (j in 1:500) rolling_apply(ts1, width=width, FUN="mean", by=by)
  Rprof(NULL)
  summaryRprof()
  
  # Profile implementation (move window in big setps)
  # -) 78% of time spent in mapply()
  Rprof(interval=0.01)
  for (j in 1:100) rolling_apply(ts1, width=width, FUN="mean")
  Rprof(NULL)
  summaryRprof()
}


