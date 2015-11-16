/* 
 * This implementation is a subset of the implementation described in
 * "Algorithms for Unevenly-Spaced Time Series", Eckner (2011).
 */

#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif


// SMA_last(X, width)
void sma_last(double values[], double times[], int *n, double values_new[], double *width)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // width      ... (positive) width of rolling window
  
  int i, left = 0;
  double t_left_new, roll_area, left_area;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Initialize output
  values_new[0] = values[0];  
  roll_area = left_area = values[0] * (*width);
  
  // Apply rolling window
  for (i = 1; i < *n; i++) {
    // Expand interval on right end
    roll_area += values[i-1] * (times[i] - times[i-1]);
    
    // Remove truncated area on left end
    roll_area = roll_area - left_area;
    
    // Shrink interval on left end
    t_left_new = times[i] - *width;
    while (times[left] < t_left_new) {
      roll_area = roll_area - values[left] * (times[left+1] - times[left]);
      left++;  
    }
    
    // Add truncated area on left end
    left_area = values[max(0, left-1)] * (times[left] - t_left_new);
    roll_area += left_area;
    
    // Save SMA value for current time window
    values_new[i] = roll_area / *width;
  }
}


// SMA_next(X, width)
void sma_next(double values[], double times[], int *n, double values_new[], double *width)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // width      ... (positive) width of rolling window
  
  int i, left = 0;
  double t_left_new, roll_area, left_area;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Initialize output
  values_new[0] = values[0];  
  roll_area = left_area = values[0] * (*width);
  
  // Apply rolling window
  for (i = 1; i < *n; i++) {
    // Expand interval on right end
    roll_area += values[i] * (times[i] - times[i-1]);
    
    // Remove truncated area on left end
    roll_area = roll_area - left_area;
    
    // Shrink interval on left end
    t_left_new = times[i] - *width;
    while (times[left] < t_left_new) {
      roll_area = roll_area - values[left+1] * (times[left+1] - times[left]);
      left++;  
    }
    
    // Add truncated area on left end
    left_area = values[left] * (times[left] - t_left_new);
    roll_area += left_area;
    
    // Save SMA value for current time window
    values_new[i] = roll_area / *width;
  }
}



// SMA_linear(X, width)
void sma_linear(double values[], double times[], int *n, double values_new[], double *width)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // width      ... (positive) width of rolling window
  
  int i, left = 0;
  double t_left_new, roll_area, left_area, truncated_width, weight, y2;
  
  // Trivial case
  if (*n == 0)
    return;
  
  // Initialize output
  values_new[0] = values[0];
  roll_area = left_area = values[0] * (*width);
  
  // Apply rolling window
  for (i = 1; i < *n; i++) {   
    // Expand interval on right end
    roll_area += (values[i] + values[i-1]) / 2 * (times[i] - times[i-1]);
    
    // Remove truncated area on left end
    roll_area = roll_area - left_area;
    
    // Shrink interval on left end
    t_left_new = times[i] - *width;
    while (times[left] < t_left_new) {
      roll_area = roll_area - (values[left] + values[left+1]) / 2 *
        (times[left+1] - times[left]);
      left++;  
    }
    
    // Add truncated area on left end
    // Inline trapezoid() functionality to avoid function call overhead
    truncated_width = times[left] - t_left_new;
    if ((left == 0) || (truncated_width == 0))
      left_area = truncated_width * values[0];
    else {
      weight = truncated_width / (times[left] - times[left-1]);
      y2 = values[left-1] * weight + values[left] * (1 - weight);
      left_area = truncated_width * (y2 + values[left]) / 2;
    }
    roll_area = roll_area + left_area;
    
    // Save SMA value for current time window
    values_new[i] = roll_area / *width;
  }
}

