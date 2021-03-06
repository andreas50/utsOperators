###################################
# Speed analysis (no comparisons) #
###################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.5.1, gcc-4.9.3

### rolling_apply (non-specialized), 8/2018
if (0) {
  ts1 <- ex_uts3()
  width <- ddays(100)
  by <- ddays(50)
  
  # Move window one observation at a time: 1.17s
  system.time(for (j in 1:100) rolling_apply(ts1, width=width, FUN="mean", use_specialized=FALSE))
  
  # Move window in big steps: 0.39s
  system.time(for (j in 1:200) rolling_apply(ts1, width=width, FUN="mean", by=by, use_specialized=FALSE))
  
  # Profile implementation (move one observation at a time)
  # -) almost all time spent on argument checking
  Rprof(interval=0.01)
  for (j in 1:500) rolling_apply(ts1, width=width, FUN="mean", by=by, use_specialized=FALSE)
  Rprof(NULL)
  summaryRprof()
  
  # Profile implementation (move window in big setps)
  # -) ~80% of time spent in mapply()
  Rprof(interval=0.01)
  for (j in 1:200) rolling_apply(ts1, width=width, FUN="mean", use_specialized=FALSE)
  Rprof(NULL)
  summaryRprof()
}
