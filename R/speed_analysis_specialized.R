#############################################################
# Speed analysis of general vs. specialized implementations #
#############################################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3


### rolling_apply_specialized vs. rolling_apply for FUN=sum
# -) the specialized implementation is ~70 times faster
# -) the results for FUN=mean are very similar, because the implementations are almost identical
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  
  # generic vs. specialized: 1.28s vs. 1.84s
  system.time(for (j in 1:200) rolling_apply(ts1, width, FUN=sum))
  system.time(for (j in 1:20000) rolling_apply_specialized(ts1, width, FUN=sum))
  
  # Profile specialized implementation
  # -) less than 15% of time spent in C code
  # -) argument checking takes most of the time
  Rprof(interval=0.01)
  for (j in 1:20000) rolling_apply_specialized(ts1, width, FUN=sum)
  Rprof(NULL)
  summaryRprof()
}


### Same, but for FUN=min/max
# -) the specialized implementation is ~70 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  
  # generic vs. specialized: 1.34s vs. 1.90s
  system.time(for (j in 1:200) rolling_apply(ts1, width, FUN=min))
  system.time(for (j in 1:20000) rolling_apply_specialized(ts1, width, FUN=min))
  
  # Profile specialized implementation
  # -) less than 15% of time spent in C code
  Rprof(interval=0.01)
  for (j in 1:20000) rolling_apply_specialized(ts1, width, FUN=min)
  Rprof(NULL)
  summaryRprof()
}


### Same, but for FUN=median
# -) the specialized implementation is ~35 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  
  # generic vs. specialized: 3.99s vs. 1.14s
  system.time(for (j in 1:100) rolling_apply(ts1, width, FUN=median))
  system.time(for (j in 1:1000) rolling_apply_specialized(ts1, width, FUN=median))
  
  # Profile specialized implementation
  # -) ~90% of time spent in C code
  Rprof(interval=0.01)
  for (j in 1:2000) rolling_apply_specialized(ts1, width, FUN=median)
  Rprof(NULL)
  summaryRprof()
}


### rolling_num_obs vs. rolling_num_obs_two_sided
# -) CONCLUSION: negligible difference -> removed one-sided version -> code does not run any more
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  width2 <- ddays(0)
  
  # one sided vs. two-sided implementation with same  window
  # -) 2.19s vs. 2.29s
  system.time(for (j in 1:50000) generic_C_interface(ts1, "rolling_num_obs", width=width))
  system.time(for (j in 1:50000) generic_C_interface(ts1, "rolling_num_obs_two_sided", width_before=width, width_after=width2))
}





