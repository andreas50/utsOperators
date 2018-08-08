// Copyright: 2012-2018 by Andreas Eckner
// License: GPL-2 | GPL-3
// Remark: To facilitate interfaces to other programming languages such as R, all variables are either pointers or arrays

#ifndef _rolling_h
#define _rolling_h

void rolling_central_moment(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after, const double *m);

void rolling_max(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void rolling_mean(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void rolling_median(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void rolling_min(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void rolling_num_obs(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void rolling_product(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void rolling_sd(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void rolling_sum(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void rolling_sum_stable(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void rolling_var(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

#endif
