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
  width <- ddays(100)
  
  # R vs. C: 0.01s vs. 2.20s
  system.time(for (j in 1:50) sma(ts1, width, type="equal"))
  system.time(for (j in 1:50) sma_equal_R(ts1, width))
  
  # Profile C implementation
  # -) ~20% of time spent in C implementation
  # -) argument checking takes up most of the time
  Rprof(interval=0.01)
  for (j in 1:2e4) sma(ts1, width, type="equal")
  Rprof(NULL)
  summaryRprof()
}


### sma(..., type="last")
# -) for a moderate-length time series, the C implementation is around 120 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  
  # R vs. C: 0.01s vs. 2.06s
  system.time(for (j in 1:200) sma(ts1, width, type="last"))
  system.time(for (j in 1:200) sma_last_R(ts1, width))
  
  # Profile C implementation
  # -) ~15% of time spent in C implementation
  # -) argument checking takes up most of the time
  Rprof(interval=0.01)
  for (j in 1:2e4) sma(ts1, width, type="last")
  Rprof(NULL)
  summaryRprof()
}


### sma(..., type="linear")
# -) for a moderate-length time series, the C implementation is around 200 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  
  # R vs. C: 0.01s vs. 3.55s
  system.time(for (j in 1:200) sma(ts1, width, type="linear"))
  system.time(for (j in 1:200) sma_linear_R(ts1, width))
  
  # Profile C implementation
  # -) ~20% of time spent in C implementation
  # -) argument checking takes up most of the time
  Rprof(interval=0.01)
  for (j in 1:2e4) sma(ts1, width, type="linear")
  Rprof(NULL)
  summaryRprof()
}


