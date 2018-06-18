#include <Rcpp.h>

extern "C" {
#include "ema.h"
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_ema_last(Rcpp::NumericVector values, Rcpp::NumericVector times, const double tau)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  ema_last(values.begin(), times.begin(), &n, res.begin(), const_cast<double*>(&tau));
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_ema_linear(Rcpp::NumericVector values, Rcpp::NumericVector times, const double tau)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  ema_linear(values.begin(), times.begin(), &n, res.begin(), const_cast<double*>(&tau));
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_ema_next(Rcpp::NumericVector values, Rcpp::NumericVector times, const double tau)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  ema_next(values.begin(), times.begin(), &n, res.begin(), const_cast<double*>(&tau));
  return res;
}

