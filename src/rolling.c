// Copyright: 2012-2016 by Andreas Eckner
// License: GPL-2 | GPL-3

#include <math.h>

#ifndef swap
#define swap(a,b) {temp=(a); (a)=(b); (b)=temp;}
#endif


/******************* Helper functions ********************/


// Return smallest element of an array (defined as +infinity for empty array)
static inline double array_min(double values[], int n)
{
  // values ... array of values
  // n      ... length of array
  
  double min_value = INFINITY;
  
  for (int i = 0; i < n; i++) {
    if (values[i] < min_value)
      min_value = values[i];
  }
  return min_value;
}


/*
Find the k-th largest element (counting starts at zero) of an array using the "quickselect" algorithm
-) O(N) average case performance
-) the input array will be rearranged
*/
double quickselect(double values[], int n, int k)
{
  // values ... array of values
  // n      ... length of array
  // k      ... return k-th smallest element
  
  if (k >= n)
    return NAN;
  
  int i, j, left, right, mid;
  double pivot, temp;
  left = 0;
  right = n - 1;
  
  // Loop invariant: values[left] <= k-th largest element of values <= values[right]
  while (1) {
    if (right - left <= 1) {
      // Candidate region down to 1-2 elements
      if ((right == left + 1) && (values[right] < values[left]))
        swap(values[left], values[right])
      return values[k];
    } else {
      // The pivot element is the second largest value of: values[left], values[mid], values[right]
      // -) avoids quadractic run-time on some common inputs, without need to pick random element
      mid = (left + right) / 2;
      swap(values[mid], values[left + 1]);
      
      // Sort the three elements from which the pivot is picked
      if (values[left] > values[right])
        swap(values[left], values[right])
      if (values[left + 1] > values[right])
        swap(values[left + 1], values[right])
      if (values[left] > values[left + 1])
        swap(values[left], values[left + 1])
      pivot = values[left + 1];
      
      // Partition the candidate region, i.e. put smaller elements to left of pivot, larger to right
      // -) the two-sided algorithm avoids quadratic run-time on some common inputs
      // -) loop invariant: elements <= i are less than the pivot, elements >= j are larger than the pivot
      // -) see Chapter 11.3 in "Programming Pearls", 2nd edition, by John Bentley
      i = left + 1;
      j = right;
      while (1) {
        do i++; while (values[i] < pivot);
        do j--; while (values[j] > pivot);
        if (j < i)
          break;
        swap(values[i], values[j])
      }
      values[left + 1] = values[j];
      values[j] = pivot;
      if (j >= k)
        right = j-1;
      if (j <= k)
        left = i;
    }
  }
}


// Find the median value of an array (which gets scrambled)
double median(double values[], int n)
{
  // values ... array of values
  // n      ... length of array

  double value_low, value_high;
  
  if (n == 0)
    return NAN;
  
  // Determine the mid points of the array
  int mid_low = (n - 1) / 2;
  int mid_high = n - mid_low - 1;
  value_low = quickselect(values, n, mid_low);
  
  if (mid_low < mid_high) {   // even number of elements -> two mid points
    // Get the smallest element to the right of lowest mid-point
    value_high = array_min(values + mid_high, n - mid_high);
    return (value_low + value_high) / 2; 
  } else
    return value_low;
}


/****************** END: Helper functions ****************/


// Rolling number of observation values
void rolling_num_obs(double values[], double times[], int *n, double values_new[],
  double *width_before, double *width_after)
{
  // values       ... array of time series values
  // times        ... array of observation times
  // n            ... number of observations, i.e. length of 'values' and 'times'
  // values_new   ... array of length *n to store output time series values
  // width_before ... (non-negative) width of rolling window before t_i
  // width_after  ... (non-negative) width of rolling window after t_i
  
  int left = 0, right = -1;
  
  for (int i = 0; i < *n; i++) {
    // Expand window on the right
    while ((right < *n - 1) && (times[right + 1] <= times[i] + *width_after))
      right++;
    
    // Shrink window on the left
    while ((left < *n) && (times[left] <= times[i] - *width_before))
      left++;
    
    // Number of observations is equal to length of window
    values_new[i] = right - left + 1;
  }
}


// Rolling sum of observation values
void rolling_sum(double values[], double times[], int *n, double values_new[],
  double *width_before, double *width_after)
{
  // values       ... array of time series values
  // times        ... array of observation times
  // n            ... number of observations, i.e. length of 'values' and 'times'
  // values_new   ... array of length *n to store output time series values
  // width_before ... (non-negative) width of rolling window before t_i
  // width_after  ... (non-negative) width of rolling window after t_i
  
  int left = 0, right = -1;
  double roll_sum = 0;
  
  for (int i = 0; i < *n; i++) {
    // Expand window on the right
    while ((right < *n - 1) && (times[right + 1] <= times[i] + *width_after)) {
      right++;
      roll_sum = roll_sum + values[right];
    }
    
    // Shrink window on the left
    while ((left < *n) && (times[left] <= times[i] - *width_before)) {
      roll_sum = roll_sum - values[left];
      left++;
    }
    
    // Update rolling sum
    values_new[i] = roll_sum;
  }
}


// Rolling product of observation values
void rolling_product(double values[], double times[], int *n, double values_new[],
  double *width_before, double *width_after)
{
  // values       ... array of time series values
  // times        ... array of observation times
  // n            ... number of observations, i.e. length of 'values' and 'times'
  // values_new   ... array of length *n to store output time series values
  // width_before ... (non-negative) width of rolling window before t_i
  // width_after  ... (non-negative) width of rolling window after t_i
  
  int left = 0, right = -1, most_recent_zero = -1;
  double roll_product = 1;
  
  for (int i = 0; i < *n; i++) {
    // Expand window on the right
    while ((right < *n - 1) && (times[right + 1] <= times[i] + *width_after)) {
      right++;
      roll_product = roll_product * values[right];
      
      // Save position of most recent zero
      if ((values[right] > -1e-10) && (values[right] < 1e-10))
        most_recent_zero = right;
    }
    
    // Shrink window on the left
    while ((left < *n) && (times[left] <= times[i] - *width_before)) {
      // Don't need to update rolling product if zero drops out, because calculated from scratch below
      if ((values[left] < -1e-10) || (values[left] > 1e-10))
        roll_product = roll_product / values[left];
      left++;
    }
    
    // Update rolling product
    // -) need to calculate from scratch in case a zero dropped out of the window
    if ((roll_product == 0) && (most_recent_zero < left)) {
      roll_product = 1;
      for (int pos=left; pos <= right; pos++)
        roll_product = roll_product * values[pos];
    }
    values_new[i] = roll_product;
  }
}


// Rolling average of observation values
void rolling_mean(double values[], double times[], int *n, double values_new[],
  double *width_before, double *width_after)
{
  // values       ... array of time series values
  // times        ... array of observation times
  // n            ... number of observations, i.e. length of 'values' and 'times'
  // values_new   ... array of length *n to store output time series values
  // width_before ... (non-negative) width of rolling window before t_i
  // width_after  ... (non-negative) width of rolling window after t_i
  
  int left = 0, right = -1;
  double roll_sum = 0;
  
  for (int i = 0; i < *n; i++) {
    // Expand window on the right
    while ((right < *n - 1) && (times[right + 1] <= times[i] + *width_after)) {
      right++;
      roll_sum = roll_sum + values[right];
    }
    
    // Shrink window on the left to get half-open interval
    while ((left < *n) && (times[left] <= times[i] - *width_before)) {
      roll_sum = roll_sum - values[left];
      left++;
    }
    
    // Calculate mean of values in rolling window
    if (left <= right)  // non-empty window
      values_new[i] = roll_sum / (right - left + 1);
    else                // empty window
      values_new[i] = NAN;
  }
}


// Rolling maximum of observation values
void rolling_max(double values[], double times[], int *n, double values_new[],
  double *width_before, double *width_after)
{
  // values       ... array of time series values
  // times        ... array of observation times matching time series values
  // n            ... length of 'values'
  // values_new   ... array (of same length as 'values') used to store output
  // width_before ... (non-negative) width of rolling window before t_i
  // width_after  ... (non-negative) width of rolling window after t_i
  
  int j, left = 0, right = -1, max_pos = 0;
  
  for (int i = 0; i < *n; i++) {
    // Expand window on the right
    while ((right < *n - 1) && (times[right + 1] <= times[i] + *width_after)) {
      right++;
      if (values[right] >= values[max_pos])
        max_pos = right;
    }
    
    // Shrink window on the left to get half-open interval
    while ((left < *n) && (times[left] <= times[i] - *width_before))
      left++;      
    
    // Recalculate position of maximum if old maximum dropped out
    // Inline functionality of max_index() to avoid function call overhead
    if (max_pos < left) {
      max_pos = left;
      for (j = left+1; j <= right; j++)
        if (values[j] >= values[max_pos])
          max_pos = j;
    }
    
    // Save maximum in current time window
    if (left <= right)  // non-empty window
      values_new[i] = values[max_pos];
    else                // empty window
      values_new[i] = -INFINITY;
  }
}


// Rolling minimum of observation values
void rolling_min(double values[], double times[], int *n, double values_new[],
  double *width_before, double *width_after)
{
  // values       ... array of time series values
  // times        ... array of observation times matching time series values
  // n            ... length of 'values'
  // values_new   ... array (of same length as 'values') used to store output
  // width_before ... (non-negative) width of rolling window before t_i
  // width_after  ... (non-negative) width of rolling window after t_i
  
  int j, left = 0, right = -1, min_pos = 0;
  
  for (int i = 0; i < *n; i++) {   
    // Expand window on the right
    while ((right < *n - 1) && (times[right + 1] <= times[i] + *width_after)) {
      right++;
      if (values[right] <= values[min_pos])
        min_pos = right;
    }
    
    // Shrink window on the left to get half-open interval
    while ((left < *n) && (times[left] <= times[i] - *width_before))
      left++;      
    
    // Recalculate position of minimum if old minimum dropped out
    // Inline the calculation of the minimum position to avoid any function call overhead
    if (min_pos < left) {
      min_pos = left;
      for (j = left+1; j <= right; j++)
        if (values[j] <= values[min_pos])
          min_pos = j;
    }
    
    // Save minium in current time window
    if (left <= right)  // non-empty window
      values_new[i] = values[min_pos];
    else                // empty window
      values_new[i] = INFINITY;
  }
}


// Rolling median
void rolling_median(double values[], double times[], int *n, double values_new[], 
  double *width_before, double *width_after)
{
  // values       ... array of time series values
  // times        ... array of observation times matching time series values
  // n            ... length of 'values'
  // values_new   ... array (of same length as 'values') used to store output
  // width_before ... (non-negative) width of rolling window before t_i
  // width_after  ... (non-negative) width of rolling window after t_i
  
  int j, window_length, left = 0, right = -1;
  double values_tmp[*n];      // temporary array for median(), which shuffles the input data 

  for (int i = 0; i < *n; i++) {
    // Expand window on the right
    while ((right < *n - 1) && (times[right + 1] <= times[i] + *width_after))
      right++;
    
    // Shrink window on the left end
    while ((left < *n) && (times[left] <= times[i] - *width_before))
      left++;
    
    // Copy data in rolling window to temporary array, then calculate the median
    window_length = right - left + 1;
    for (j = 0; j < window_length; j++)
      values_tmp[j] = values[left + j];
    values_new[i] = median(values_tmp, window_length);
  }
}
