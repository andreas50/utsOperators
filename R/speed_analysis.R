#############################################
# Speed analysis of R vs. C implementations #
#############################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3

### sma(..., type="equal")
# -) for long time series and long moving average window, the C implementation is around 4 times faster
# -) excluding the overhead for argument checking, the C implementation is 30-60 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1e4), as.POSIXct("2000-01-01") + ddays(0:1e4))
  tau <- ddays(1000)
  
  # R vs. C: 0.58s vs. 2.35s
  system.time(for (j in 1:1e4) sma(ts1, tau, type="equal"))
  system.time(for (j in 1:1e4) sma_equal_R(ts1, tau))
  
  # Profile C implementation
  # -) only ~7% of time spent in C implementation
  # -) argument checking takes up most of the time
  Rprof(interval=0.01)
  for (j in 1:5e4) sma(ts1, tau, type="equal")
  Rprof(NULL)
  summaryRprof()
  
  # Profile R implementation
  Rprof(interval=0.01)
  for (j in 1:1e4) sma_equal_R(ts1, tau)
  Rprof(NULL)
  summaryRprof()
}
