/* 
 * This implementation is a subset of the implementation described in
 * "Algorithms for Unevenly-Spaced Time Series", Eckner (2011).
 */

#include <stdlib.h>

#ifndef swap
#define swap(a,b) {temp=(a); (a)=(b); (b)=temp;}
#endif


/******************* Helper functions ********************/


// Return smallest element of an array
double array_min(double values[], int n)
{
  // values ... array of values
  // n      ... length of array
  
  double min_value = values[0];    
  
  for (int i = 1; i < n; i++) {
    if (values[i] < min_value)
      min_value = values[i];
  }
  return min_value;
}


/*
Find the k-th largest (counting starts at zero) of an array using the "quickselect" algorithm
-) O(N) average case performance
-) the input array will be rearranged
*/
double quickselect(double values[], int n, int k)
{
  // values ... array of values
  // n      ... length of array
  // k      ... return k-th smallest element
  
  int i, j, left, right, mid;
  double pivot, temp;
  left = 0;
  right = n - 1;
  
  while (1 > 0) {
    if (right <= left + 1) {
      // Array down to 1-2 elements
      if ((right == left + 1) && (values[right] < values[left]))
        swap(values[left], values[right])
      return values[k];
    } else {
      // Select pivot element
      mid = (left + right) / 2;   // integer devision
      swap(values[mid], values[left + 1]);
      
      // Put pivot element and the elements at the left and right boundary in the correct relative order
      if (values[left] > values[right])
        swap(values[left], values[right])
      if (values[left + 1] > values[right])
        swap(values[left + 1], values[right])
      if (values[left] > values[left + 1])
        swap(values[left], values[left + 1])
      
      // Put smaller elements to left of pivot, larger to right
      i = left + 1;
      j = right;
      pivot = values[left + 1];
      for (;;) {
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
void rolling_max(double values[], double times[], int *n, double values_new[], double *width)
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
  
  int i, j, left = 0, min_pos = 0;
  
  for (i = 0; i < *n; i++) {   
    // Expand window on the right
    if (values[i] <= values[min_pos])
      min_pos = i;
    
    // Shrink window on the left to get half-open interval
    while (times[left] <= times[i] - *width)
      left++;      
    
    // Recalculate position of minimum if old minimum dropped out
    // Inline the calculation of the minimum position to avoid any function call overhead
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


// Rolling median
void rolling_median(double values[], double times[], int *n, double values_new[], double *width)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // width      ... (positive) width of the rolling window
  
  int j, window_length, left=0;
  double values_tmp[*n];      // temporary array for median(), which shuffles the input data 

  for (int i = 0; i < *n; i++) {   
    // Shrink window on the left end
    while (times[left] <= times[i] - *width)
      left++;
    
    // Copy data in rolling window to temporary array, and calculate the median
    window_length = i - left + 1;
    for (j = 0; j < window_length; j++)
      values_tmp[j] = values[left + j];
    values_new[i] = median(values_tmp, window_length);
  }
}

