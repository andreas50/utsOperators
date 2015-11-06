###################################
# Speed analysis (no comparisons) #
###################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3

### rolling_apply
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  by <- ddays(50)
  
  # Move window one observation at a time: 1.12s
  system.time(for (j in 1:100) rolling_apply(ts1, width=width, FUN="mean"))
  
  # Move window in big steps: 0.52s
  system.time(for (j in 1:200) rolling_apply(ts1, width=width, FUN="mean", by=by))
  
  # Profile implementation (move one observation at a time)
  # -) almost all time spent on argument checking
  Rprof(interval=0.01)
  for (j in 1:500) rolling_apply(ts1, width=width, FUN="mean", by=by)
  Rprof(NULL)
  summaryRprof()
  
  # Profile implementation (move window in big setps)
  # -) ~80% of time spent in mapply()
  Rprof(interval=0.01)
  for (j in 1:100) rolling_apply(ts1, width=width, FUN="mean")
  Rprof(NULL)
  summaryRprof()
}
