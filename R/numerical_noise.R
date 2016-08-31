##################################################
# Examine numerical noise across implementations #
##################################################

# Very long long time series
# -) the R implementations is free of numerical noise, because the value for each window is calculated from scratch
if (0) {
  x <- ex_uts3(10000) ^ 4
  #x <- uts(values=rep(c(1e6, 1.2, 1.3, 1.4, 1e-10), length=1e4),  times=as.POSIXct("2016-01-01") + days(1:1e4))
  width <- ddays(10)
  
  # rolling_sum in R vs. roll_sum in C
  # -) the R & C implementation diverge over time.
  # -) the problem becomes more serious for longer, heavily skewed time series
  tmp <- rolling_apply_specialized(x, width, FUN=sum) / rolling_apply(x, width, FUN=sum, use_specialized=FALSE) - 1
  plot(tmp)
  sd(tmp)
  
  # rolling_sum in R vs. roll_sum_stable in C
  # -) standard deviation of differences ~65% lower
  tmp <- rolling_apply_specialized(x, width, FUN="sum_stable") / rolling_apply(x, width, FUN=sum, use_specialized=FALSE) - 1
  plot(tmp)
  sd(tmp)
  
  # sum() and kahansum() give the same result
  library(cmna)
  tmp <- rolling_apply(x, width, FUN=kahansum) / rolling_apply(x, width, FUN=sum, use_specialized=FALSE) - 1
  plot(tmp)
  sd(tmp)
}


# Time series with extremely skewed observation values
if (0) {
  x <- uts(values=rep(c(1e16, 1, -1e16), 10), times=as.POSIXct("2016-01-01") + days(1:30))
  width <- ddays(0.5)

  # rolling_sum in R vs. roll_sum in C
  # -) the intermediate quantity in C (roll_sum) is too large relative to the scale of some observation values, which leads to "catastrophic cancellation", in this case, relative errors of up to 100%.
  rolling_apply(x, width, FUN=sum, use_specialized=FALSE) - rolling_apply_specialized(x, width, FUN=sum)
  
  # rolling_sum in R vs. roll_sum_stable in C
  # -) no error -> good!
  rolling_apply(x, width, FUN="sum_stable") - rolling_apply_specialized(x, width, FUN=sum)
}


# Remark:
# -) the R and C implementation of SMA_last and SMA_linear both suffer from numerical noise. Therefore, one cannot compare their output (unlike for rolling_mean) to determine the extent of numeric noise.
