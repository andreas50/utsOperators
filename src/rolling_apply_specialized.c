/* 
 * This implementation is a subset of the implementation described in
 * "Algorithms for Unevenly-Spaced Time Series", Eckner (2011).
 */


// Rolling sum of observation values
void rolling_sum(double values[], double times[], int *n, double values_new[], double *width)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // width      ... (positive) width of rolling window
  
  int i, left = 0;
  double roll_sum = 0;
  
  for (i = 0; i < *n; i++) {
    // Expand window on the right
    roll_sum = roll_sum + values[i];
    
    // Shrink window on the left
    while (times[left] <= times[i] - *width) {
      roll_sum = roll_sum - values[left];
      left++;
    }
    
    // Update rolling sum
    values_new[i] = roll_sum;
  }
}


// Rolling number of observation values
void rolling_num_obs(double values[], double times[], int *n, double values_new[], double *width)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // width      ... (positive) width of rolling window
  
  int i, left = 0;

  for (i = 0; i < *n; i++) {   
    // Shrink window on the left
    while (times[left] <= times[i] - *width)
      left++;
    
    // Number of observations is equal to length of window
    values_new[i] = i - left + 1;
  }
}


// Rolling maximum of observation values
void rolling_max(double *values, double times[], int *n, double values_new[], double *width)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // width      ... (positive) width of rolling window
  
  int i, j, left = 0, max_pos = 0;
  
  for (i = 0; i < *n; i++) {   
    // Expand window on the right
    if (values[i] >= values[max_pos])
      max_pos = i;
    
    // Shrink window on the left to get half-open interval
    while (times[left] <= times[i] - *width)
      left++;      
    
    // Recalculate position of maximum if old maximum dropped out
    // Inline functionality of max_index() to avoid function call overhead
    if (max_pos < left) {
      max_pos = left;
      for (j = left+1; j <= i; j++)
        if (values[j] >= values[max_pos])
          max_pos = j;
    }
    
    // Save maxium for current time window
    values_new[i] = values[max_pos];
  }
}


// Rolling minimum of observation values
void rolling_min(double values[], double times[], int *n, double values_new[], double *width)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // width      ... (positive) width of rolling window
  
  int i, j, left = 0, min_pos=0;
  
  for (i = 0; i < *n; i++) {   
    // Expand window on the right
    if (values[i] <= values[min_pos])
      min_pos = i;
    
    // Shrink window on the left to get half-open interval
    while (times[left] <= times[i] - *width)
      left++;      
    
    // Recalculate position of minimum if old minimum dropped out
    // Inline functionality of min_index() to avoid function call overhead
    if (min_pos < left) {
      min_pos = left;
      for (j = left+1; j <= i; j++)
        if (values[j] <= values[min_pos])
          min_pos = j;
    }
    
    // Save minium for current time window
    values_new[i] = values[min_pos];
  }
}

