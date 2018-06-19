
<!-- README.md is generated from README.Rmd. Please edit that file -->
### Introduction

This package provides rolling time series operators for unevenly spaced data, such as simple moving averages (SMAs), exponential moving averages (EMAs), and arbitrary rolling R functions. It is a wrapper around the highly-optimized C library [utsAlgorithms](https://github.com/andreas50/utsAlgorithms). The time series class used by this package is the [uts](https://github.com/andreas50/uts) class.

The package [rcpputs](https://github.com/eddelbuettel/rcpputs) is a low-level wrapper around the same C library that does not rely on any time series class, but instead requires the user to pass in a vector of observation values and observation times to each function.

### Installation

This package is not yet available on CRAN, but can be installled from GitHub:

``` r
devtools::install_github(c("andreas50/uts", "andreas50/utsOperators"))  # using package 'devtools'
remotes::install_github(c("andreas50/uts", "andreas50/utsOperators"))   # ... or using package 'remotes'
```

### Sample Code

``` r
# Get sample unevenly-spaced time series with six observations
x <- ex_uts()
x
#> 2007-11-08 07:00:00 2007-11-08 08:01:00 2007-11-08 13:15:00 2007-11-09 07:30:00 2007-11-09 08:51:00 
#>              48.375              48.500              48.375              47.000              47.500 
#> 2007-11-09 15:15:00 
#>              47.350
```

``` r
# SMA with last-point interpolation, 1-day wide rolling time window
sma(x, ddays(1))

# EMA with linear interpolation, 12-hour effective temporal length
ema(x, dhours(12), interpolation="linear")

# Rolling mean, sum, number of observation values in a 1-day wide rolling time window
rolling_apply(ex_uts(), width=ddays(1), FUN=mean)
rolling_apply(ex_uts(), width=ddays(1), FUN=sum)
rolling_apply(ex_uts(), width=ddays(1), FUN=length)
```
