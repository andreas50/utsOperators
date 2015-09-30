##################################################
# Examine numerical noise across implementations #
##################################################

# SMA_equal R vs. C implementation
# -) the R implementations is free of numerical noise, because each SMA value is calculated from scratch
if (0) {
  set.seed(1)
  x <- uts(rnorm(10000) ^ 3, as.POSIXct("2000-01-01") + ddays(1:10000))
  tau <- ddays(3)
  
  # The R & C implementation diverge over time.
  # -) the problem becomes more serious for longer, heavily skewed time series
  plot(sma(x, tau, type="equal") - sma_equal_R(x, tau))
}


# Remark:
# -) the R and C implementation of SMA_last and SMA_linear both suffer from numerical noise. Therefore, one cannot compare their output (unlike for SMA_equal) to determine the extent of numeric noise.
