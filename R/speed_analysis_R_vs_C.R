#############################################
# Speed analysis of R vs. C implementations #
#############################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3

### sma(..., interpolation="last")
# -) for a moderate-length time series, the C implementation is ~110 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  
  # R vs. C: 1.92s vs. 1.73s
  system.time(for (j in 1:200) sma_last_R(ts1, width))
  system.time(for (j in 1:20000) sma(ts1, width, interpolation="last"))
  
  # Profile C implementation
  # -) ~20% of time spent in C implementation
  # -) argument checking takes up most of the time
  Rprof(interval=0.01)
  for (j in 1:5e4) sma(ts1, width, interpolation="last")
  Rprof(NULL)
  summaryRprof()
}


### sma(..., interpolation="linear")
# -) for a moderate-length time series, the C implementation is ~180 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  
  # R vs. C: 3.28s vs. 1.83s
  system.time(for (j in 1:200) sma_linear_R(ts1, width))
  system.time(for (j in 1:20000) sma(ts1, width, interpolation="linear"))
  
  # Profile C implementation
  # -) ~20% of time spent in C implementation
  # -) argument checking takes up most of the time
  Rprof(interval=0.01)
  for (j in 1:5e4) sma(ts1, width, interpolation="linear")
  Rprof(NULL)
  summaryRprof()
}


