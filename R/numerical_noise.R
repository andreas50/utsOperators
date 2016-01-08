##################################################
# Examine numerical noise across implementations #
##################################################

# rolling_mean R vs. C implementation
# -) the R implementations is free of numerical noise, because each SMA value is calculated from scratch
if (0) {
  x <- ex_uts3(10000) ^ 3
  width <- ddays(3)
  
  # The R & C implementation diverge over time.
  # -) the problem becomes more serious for longer, heavily skewed time series
  plot(rolling_apply(x, width, FUN=mean, use_specialized=FALSE) - rolling_apply_specialized(x, width, FUN=mean))
}


# Remark:
# -) the R and C implementation of SMA_last and SMA_linear both suffer from numerical noise. Therefore, one cannot compare their output (unlike for SMA_equal) to determine the extent of numeric noise.
