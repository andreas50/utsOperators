#include <Rcpp.h>

extern "C" {
#include "sma.h"
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_sma_last(Rcpp::NumericVector values, Rcpp::NumericVector times,
  const double width_before, const double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  sma_last(values.begin(), times.begin(), &n, res.begin(),
    const_cast<double*>(&width_before), const_cast<double*>(&width_after));
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_sma_linear(Rcpp::NumericVector values, Rcpp::NumericVector times,
  const double width_before, const double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  sma_linear(values.begin(), times.begin(), &n, res.begin(),
    const_cast<double*>(&width_before), const_cast<double*>(&width_after));
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_sma_next(Rcpp::NumericVector values, Rcpp::NumericVector times,
  const double width_before, const double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  sma_next(values.begin(), times.begin(), &n, res.begin(),
    const_cast<double*>(&width_before), const_cast<double*>(&width_after));
  return res;
}
