#############################################################
# Speed analysis of general vs. specialized implementations #
#############################################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3

### sma(..., type="equal") vs. rolling_apply(..., FUN="mean")
# -) the specialized implementation is ~150 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  tau <- ddays(100)
  
  # SMA vs. rolling_apply: 0.81s vs. 1.2s
  system.time(for (j in 1:10000) sma(ts1, tau, type="equal"))
  system.time(for (j in 1:100) rolling_apply(ts1, tau, FUN=mean))
}


### rolling_sum vs. rolling_apply(..., FUN="sum")
# -) the specialized implementation is ~150 times faster
if (0) {
  set.seed(1)
  ts1 <- uts(rnorm(1000), as.POSIXct("2000-01-01") + ddays(1:1000))
  tau <- ddays(100)
}