#include <Rcpp.h>

extern "C" {
#include "sma.h"
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_sma_last(const Rcpp::NumericVector& values, const Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  sma_last(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_sma_linear(const Rcpp::NumericVector& values, const Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  sma_linear(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_sma_next(const Rcpp::NumericVector& values, const Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  sma_next(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}
