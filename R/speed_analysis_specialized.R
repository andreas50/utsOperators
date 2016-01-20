#############################################################
# Speed analysis of general vs. specialized implementations #
#############################################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3


### rolling_apply_specialized vs. rolling_apply for FUN=sum
# -) the specialized implementation is ~50 times faster
# -) the results for FUN=mean are very similar, because the implementations are almost identical
if (0) {
  ts1 <- ex_uts3()
  width <- ddays(100)
  
  # generic vs. specialized: 1.24s vs. 2.61s
  system.time(for (j in 1:200) rolling_apply(ts1, width, FUN=sum, use_specialized=FALSE))
  system.time(for (j in 1:20000) rolling_apply(ts1, width, FUN=sum))
  
  # Profile specialized implementation
  # -) ~15% of time spent in C code
  # -) argument checking takes most of the time
  Rprof(interval=0.01)
  for (j in 1:50000) rolling_apply(ts1, width, FUN=sum)
  Rprof(NULL)
  summaryRprof()

 # Profile generic implementation
  Rprof(interval=0.01)
  for (j in 1:500) rolling_apply(ts1, width, FUN=sum, use_specialized=FALSE)
  Rprof(NULL)
  summaryRprof()
}


### Same, but for FUN=min/max
# -) the specialized implementation is ~50 times faster
if (0) {
  ts1 <- ex_uts3()
  width <- ddays(100)
  
  # generic vs. specialized: 1.31s vs. 2.53s
  system.time(for (j in 1:200) rolling_apply(ts1, width, FUN=min, use_specialized=FALSE))
  system.time(for (j in 1:20000) rolling_apply(ts1, width, FUN=min))
  
  # Profile specialized implementation
  # -) ~20% of time spent in C code
  Rprof(interval=0.01)
  for (j in 1:50000) rolling_apply(ts1, width, FUN=min)
  Rprof(NULL)
  summaryRprof()
}


### Same, but for FUN=median
# -) the specialized implementation is ~35 times faster
if (0) {
  ts1 <- ex_uts3()
  width <- ddays(100)
  
  # generic vs. specialized: 4.08s vs. 1.15s
  system.time(for (j in 1:100) rolling_apply(ts1, width, FUN=median, use_specialized=FALSE))
  system.time(for (j in 1:1000) rolling_apply_specialized(ts1, width, FUN=median))
  
  # With and without inlined helper functions: could not detect speed difference
  system.time(for (j in 1:2000) rolling_apply_specialized(ts1, width, FUN=median))
  
  # Profile specialized implementation
  # -) ~85% of time spent in C code
  Rprof(interval=0.01)
  for (j in 1:2000) rolling_apply(ts1, width, FUN=median)
  Rprof(NULL)
  summaryRprof()
}


### rolling_num_obs vs. rolling_num_obs_two_sided
# -) CONCLUSION: negligible difference -> removed one-sided version -> code does not run any more
if (0) {
  ts1 <- ex_uts3()
  width <- ddays(100)
  width2 <- ddays(0)
  
  # one sided vs. two-sided implementation with same  window
  # -) 2.19s vs. 2.29s
  system.time(for (j in 1:50000) generic_C_interface(ts1, "rolling_num_obs", width=width))
  system.time(for (j in 1:50000) generic_C_interface(ts1, "rolling_num_obs_two_sided", width_before=width, width_after=width2))
}






