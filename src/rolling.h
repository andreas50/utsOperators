// Copyright: 2012-2016 by Andreas Eckner
// License: GPL-2 | GPL-3
// Remark: To facilitate interfaces to other programming languages such as R, all variables are either pointers or arrays

#ifndef _rolling_h
#define _rolling_h

void rolling_central_moment(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after, double *m);
void rolling_max(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void rolling_mean(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void rolling_median(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void rolling_min(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void rolling_num_obs(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void rolling_product(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void rolling_sd(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void rolling_sum(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void rolling_sum_stable(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void rolling_var(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);

#endif
