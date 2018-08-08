// Copyright: 2012-2018 by Andreas Eckner
// License: GPL-2 | GPL-3

#include <math.h>
#include "ema.h"


// EMA_next(X, tau)
void ema_next(const double values[], const double times[], const int *n, double values_new[], const double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) half-life of EMA kernel
  
  double w;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Calculate ema recursively
  values_new[0] = values[0];
  for (int i = 1; i < *n; i++) {
    w = exp(-(times[i] - times[i-1]) / *tau);
    values_new[i] = values_new[i-1] * w + values[i] * (1-w);
  }
}


// EMA_last(X, tau)
void ema_last(const double values[], const double times[], const int *n, double values_new[], const double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) half-life of EMA kernel
  
  double w;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Calculate ema recursively   
  values_new[0] = values[0];
  for (int i = 1; i < *n; i++) {
    w = exp(-(times[i] - times[i-1]) / *tau);
    values_new[i] = values_new[i-1] * w + values[i-1] * (1-w);
  }
  
}


// EMA_lin(X, tau)
void ema_linear(const double values[], const double times[], const int *n, double values_new[], const double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) half-life of EMA kernel
  
  double w, w2, tmp;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Calculate ema recursively   
  values_new[0] = values[0];   
  for (int i = 1; i < *n; i++) {
    tmp = (times[i] - times[i-1]) / *tau;
    w = exp(-tmp);
    if (tmp > 1e-6)
      w2 = (1 - w) / tmp;
    else {
      // Use Taylor expansion for numerical stability
      w2 = 1 - tmp/2 + tmp*tmp/6 - tmp*tmp*tmp/24;
    }
    values_new[i] = values_new[i-1] * w + values[i] * (1 - w2) + values[i-1] * (w2 - w);
  }
}
