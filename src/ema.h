// Copyright: 2012-2018 by Andreas Eckner
// License: GPL-2 | GPL-3
// Remark: To facilitate interfaces to other programming languages such as R, all variables are either pointers or arrays

#ifndef _ema_h
#define _ema_h

void ema_next(const double values[], const double times[], const int *n, double values_new[], const double *tau);
void ema_last(const double values[], const double times[], const int *n, double values_new[], const double *tau);
void ema_linear(const double values[], const double times[], const int *n, double values_new[], const double *tau);

#endif
