// Copyright: 2012-2018 by Andreas Eckner
// License: GPL-2 | GPL-3
// Remark: To facilitate interfaces to other programming languages such as R, all variables are either pointers or arrays

#ifndef _sma_h
#define _sma_h

void sma_last(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void sma_next(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

void sma_linear(const double values[], const double times[], const int *n, double values_new[],
  const double *width_before, const double *width_after);

#endif
