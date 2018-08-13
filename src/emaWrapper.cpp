#include <Rcpp.h>

extern "C" {
#include "ema.h"
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_ema_last(const Rcpp::NumericVector& values, const Rcpp::DatetimeVector& times, double tau)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  ema_last(values.begin(), times.begin(), &n, res.begin(), &tau);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_ema_linear(const Rcpp::NumericVector& values, const Rcpp::DatetimeVector& times, double tau)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  ema_linear(values.begin(), times.begin(), &n, res.begin(), &tau);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_ema_next(const Rcpp::NumericVector& values, const Rcpp::DatetimeVector& times, double tau)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  ema_next(values.begin(), times.begin(), &n, res.begin(), &tau);
  return res;
}

