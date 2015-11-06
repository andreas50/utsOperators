#############################################################
# Speed analysis of general vs. specialized implementations #
#############################################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3

### sma(..., type="equal") vs. rolling_apply(..., FUN="mean")
# -) the specialized implementation is ~140 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  tau <- ddays(100)
  
  # gneric vs. specialized: 2.37s vs. 1.66s
  system.time(for (j in 1:200) rolling_apply(ts1, tau, FUN=mean))
  system.time(for (j in 1:20000) sma(ts1, tau, type="equal"))
}


### rolling_sum vs. rolling_apply(..., FUN="sum")
# -) the specialized implementation is 50-60 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  width <- ddays(100)
  
  # gneric vs. specialized: 1.31s vs. 2.28s
  system.time(for (j in 1:200) rolling_apply(ts1, width, FUN="sum"))
  system.time(for (j in 1:20000) rolling_sum.uts(ts1, width))
}
