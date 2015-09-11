/* 
 * This implementation is a subset of the implementation described in
 * "Algorithms for Unevenly-Spaced Time Series", Eckner (2011).
 */

#include <math.h>

#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif



/*********************************
 * Simple Moving Averages (SMAs) *
 ********************************/

// SMA_equal(X, tau)
void sma_equal(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) width of rolling window
   
  int i, left = 0;
  double roll_sum = 0;
  
  for (i = 0; i < *n; i++) {
    // Expand window on right
    roll_sum = roll_sum + values[i];
    
    // Shrink window on the left to get half-open interval
    while (times[left] <= times[i] - *tau) {
      roll_sum = roll_sum - values[left];
      left++;
    }
   
    // Calculate mean of values in rolling window
    values_new[i] = roll_sum / (i - left + 1);
  }
}


// SMA_last(X, tau)
void sma_last(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) width of rolling window
  
  int i, left = 0;
  double t_left_new, roll_area, left_area;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Initialize output
  values_new[0] = values[0];  
  roll_area = left_area = values[0] * (*tau);
  
  // Apply rolling window
  for (i = 1; i < *n; i++) {
    // Expand interval on right end
    roll_area += values[i-1] * (times[i] - times[i-1]);
    
    // Remove truncated area on left end
    roll_area = roll_area - left_area;
    
    // Shrink interval on left end
    t_left_new = times[i] - *tau;
    while (times[left] < t_left_new) {
      roll_area = roll_area - values[left] * (times[left+1] - times[left]);
      left++;  
    }
    
    // Add truncated area on left end
    left_area = values[max(0, left-1)] * (times[left] - t_left_new);
    roll_area += left_area;
    
    // Save SMA value for current time window
    values_new[i] = roll_area / *tau;
  }
}


// SMA_next(X, tau)
void sma_next(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) width of rolling window
  
  int i, left = 0;
  double t_left_new, roll_area, left_area;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Initialize output
  values_new[0] = values[0];  
  roll_area = left_area = values[0] * (*tau);
  
  // Apply rolling window
  for (i = 1; i < *n; i++) {
    // Expand interval on right end
    roll_area += values[i] * (times[i] - times[i-1]);
    
    // Remove truncated area on left end
    roll_area = roll_area - left_area;
    
    // Shrink interval on left end
    t_left_new = times[i] - *tau;
    while (times[left] < t_left_new) {
      roll_area = roll_area - values[left+1] * (times[left+1] - times[left]);
      left++;  
    }
    
    // Add truncated area on left end
    left_area = values[left] * (times[left] - t_left_new);
    roll_area += left_area;
    
    // Save SMA value for current time window
    values_new[i] = roll_area / *tau;
  }
}



// SMA_linear(X, tau)
void sma_linear(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) width of rolling window
  
  int i, left = 0;
  double t_left_new, roll_area, left_area, width, weight, y2;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Initialize output
  values_new[0] = values[0];
  roll_area = left_area = values[0] * (*tau);
  
  // Apply rolling window
  for (i = 1; i < *n; i++) {   
    // Expand interval on right end
    roll_area += (values[i] + values[i-1]) / 2 * (times[i] - times[i-1]);
    
    // Remove truncated area on left end
    roll_area = roll_area - left_area;
    
    // Shrink interval on left end
    t_left_new = times[i] - *tau;
    while (times[left] < t_left_new) {
      roll_area = roll_area - (values[left] + values[left+1]) / 2 *
        (times[left+1] - times[left]);
      left++;  
    }
    
    // Add truncated area on left end
    // Inline trapezoid() functionality to avoid function call overhead
    width = times[left] - t_left_new;
    if ((left == 0) | (width == 0))
      left_area = width * values[0];
    else {
      weight = width / (times[left] - times[left-1]);
      y2 = values[left-1] * weight + values[left] * (1 - weight);
      left_area = width * (y2 + values[left]) / 2;
    }
    roll_area = roll_area + left_area;
    
    // Save SMA value for current time window
    values_new[i] = roll_area / *tau;
  }
}



/**************************************
 * Exponential Moving Averages (SMAs) *
 *************************************/

// EMA_next(X, tau)
void ema_next(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) half-life of EMA kernel
  
  int i;
  double w;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Calculate ema recursively
  values_new[0] = values[0];
  for (i = 1; i < *n; i++) {
    w = exp(-(times[i]-times[i-1]) / *tau);
    values_new[i] = values_new[i-1] * w + values[i] * (1-w);
  }
}


// EMA_last(X, tau)
void ema_last(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) half-life of EMA kernel
  
  int i;
  double w;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Calculate ema recursively   
  values_new[0] = values[0];
  for (i = 1; i < *n; i++) {
    w = exp(-(times[i]-times[i-1]) / *tau);
    values_new[i] = values_new[i-1] * w + values[i-1] * (1-w);
  }
  
}


// EMA_lin(X, tau)
void ema_linear(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... (positive) half-life of EMA kernel
  
  int i;
  double w, w2, tmp;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Calculate ema recursively   
  values_new[0] = values[0];   
  for (i = 1; i < *n; i++) {
    tmp = (times[i] - times[i-1]) / *tau;
    w = exp(-tmp);
    if (tmp > 1e-6)
      w2 = (1 - w) / tmp;
    else {
      // Use Taylor expansion for numerical stabiliy
      w2 = 1 - tmp/2 + tmp*tmp/6 - tmp*tmp*tmp/24;
    }
    values_new[i] = values_new[i-1] * w + values[i] * (1 - w2) + values[i-1] * (w2 - w);
  }
}
