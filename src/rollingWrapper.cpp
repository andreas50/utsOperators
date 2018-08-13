#include <Rcpp.h>

extern "C" {
#include "rolling.h"
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_central_moment(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after, double m)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_central_moment(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after, &m);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_max(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_max(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_mean(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_mean(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_median(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_median(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_min(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_min(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_num_obs(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_num_obs(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_product(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_product(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_sd(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_sd(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_sum(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_sum(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_sum_stable(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_sum_stable(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_wrapper_rolling_var(Rcpp::NumericVector& values, Rcpp::DatetimeVector& times,
  double width_before, double width_after)
{
  // Allocate memory for output
  int n = values.size();
  Rcpp::NumericVector res(n);
  
  // Call C function
  rolling_var(values.begin(), times.begin(), &n, res.begin(), &width_before, &width_after);
  return res;
}
