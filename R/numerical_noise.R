##################################################
# Examine numerical noise across implementations #
##################################################

# rolling_sum R vs. C implementation
# -) the R implementations is free of numerical noise, because the value for each window is calculated from scratch
if (0) {
  x <- ex_uts3(10000) ^ 4
  width <- ddays(10)
  
  # The R & C implementation diverge over time.
  # -) the problem becomes more serious for longer, heavily skewed time series
  plot(rolling_apply_specialized(x, width, FUN=sum) / rolling_apply(x, width, FUN=sum, use_specialized=FALSE) - 1)
}


# same, but with extremely skewed observation values
if (0) {
  x <- uts(rep(c(1e16, 1, -1e16), 10), times=as.POSIXct("2016-01-01") + days(1:30))
  width <- ddays(0.5)

  # Conclusion: The intermediate quantity in C (roll_sum) is too large relative to the scale of some observation values, which leads to "catastrophic cancellation", in this case, relative errors of up to 100%.
  rolling_apply(x, width, FUN=sum, use_specialized=FALSE) - rolling_apply_specialized(x, width, FUN=sum)
}


# Remark:
# -) the R and C implementation of SMA_last and SMA_linear both suffer from numerical noise. Therefore, one cannot compare their output (unlike for rolling_mean) to determine the extent of numeric noise.
