#############################################################
# Speed analysis of general vs. specialized implementations #
#############################################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.5.1, gcc-4.9.3
# Date: 8/2018

### rolling_apply_specialized vs. rolling_apply for FUN=sum
# -) the specialized implementation is ~35 times faster
# -) the results for FUN=mean are very similar, because the implementations are almost identical
if (0) {
  x <- ex_uts3()
  width <- ddays(100)
  
  # generic vs. specialized: 0.89s vs. 2.59s
  system.time(for (j in 1:200) rolling_apply(x, width, FUN=sum, use_specialized=FALSE))
  system.time(for (j in 1:20000) rolling_apply(x, width, FUN=sum))
  
  # Profile specialized implementation
  # -) !10% of time spent in C code
  # -) argument checking takes most of the time
  Rprof(interval=0.01)
  for (j in 1:50000) rolling_apply(x, width, FUN=sum)
  Rprof(NULL)
  summaryRprof()

 # Profile generic implementation
  Rprof(interval=0.01)
  for (j in 1:500) rolling_apply(x, width, FUN=sum, use_specialized=FALSE)
  Rprof(NULL)
  summaryRprof()
}


### Same, but for FUN=min/max
# -) the specialized implementation is ~37 times faster
if (0) {
  x <- ex_uts3()
  width <- ddays(100)
  
  # generic vs. specialized: 0.95s vs. 2.54s
  system.time(for (j in 1:200) rolling_apply(x, width, FUN=min, use_specialized=FALSE))
  system.time(for (j in 1:20000) rolling_apply(x, width, FUN=min))
  
  # Profile specialized implementation
  # -) ~10% of time spent in C code
  Rprof(interval=0.01)
  for (j in 1:50000) rolling_apply(x, width, FUN=min)
  Rprof(NULL)
  summaryRprof()
}


### Same, but for FUN=median
# -) the specialized implementation is ~45 times faster
if (0) {
  x <- ex_uts3()
  width <- ddays(100)
  
  # generic vs. specialized: 5.63s vs. 1.20s
  system.time(for (j in 1:100) rolling_apply(x, width, FUN=median, use_specialized=FALSE))
  system.time(for (j in 1:1000) rolling_apply_specialized(x, width, FUN=median))
  
  # With and without inlined helper functions: could not detect speed difference
  system.time(for (j in 1:2000) rolling_apply_specialized(x, width, FUN=median))
  
  # Profile specialized implementation
  # -) ~90% of time spent in C code
  Rprof(interval=0.01)
  for (j in 1:5000) rolling_apply(x, width, FUN=median)
  Rprof(NULL)
  summaryRprof()
}


### rolling_sum vs. rolling_sum_stable
if (0) {
  x <- ex_uts3()
  width <- ddays(100)
  
  # 1.92s vs. 1.94s
  # -) almost no speed difference, because only 10-15% of time time is spent in the C code
  system.time(for (j in 1:2e4) rolling_apply_specialized(x, width, FUN="sum"))
  system.time(for (j in 1:2e4) rolling_apply_specialized(x, width, FUN="sum_stable"))
}
