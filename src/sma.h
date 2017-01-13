// Copyright: 2012-2017 by Andreas Eckner
// License: GPL-2 | GPL-3
// Remark: To facilitate interfaces to other programming languages such as R, all variables are either pointers or arrays

#ifndef _sma_h
#define _sma_h

void sma_last(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void sma_next(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);
void sma_linear(double values[], double times[], int *n, double values_new[], double *width_before, double *width_after);

#endif
