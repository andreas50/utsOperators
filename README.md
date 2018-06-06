
<!-- README.md is generated from README.Rmd. Please edit that file -->
### Introduction

### Overview

This package provides rolling time series operators for unevenly spaced data, such as simple moving averages (SMAs), exponential moving averages (EMAs), and arbitrary rolling R functions.

The package is a wrapper around ...

### Installation

This package is not yet available on CRAN, but can be installled from GitHub:

``` r
devtools::install_github("andreas50/utsOperators")   # using package 'devtools'
remotes::install_github("andreas50/utsOperators")    # ... or using package 'remotes'
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
