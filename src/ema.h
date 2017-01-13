// Copyright: 2012-2017 by Andreas Eckner
// License: GPL-2 | GPL-3
// Remark: To facilitate interfaces to other programming languages such as R, all variables are either pointers or arrays

#ifndef _ema_h
#define _ema_h

void ema_next(double values[], double times[], int *n, double values_new[], double *tau);
void ema_last(double values[], double times[], int *n, double values_new[], double *tau);
void ema_linear(double values[], double times[], int *n, double values_new[], double *tau);

#endif
