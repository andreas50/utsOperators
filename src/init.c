#include <stdlib.h>
#include <R_ext/Rdynload.h>

#include "ema.h"
#include "sma.h"
#include "rolling.h"
#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
   // EMA functions
   CALLDEF(ema_next, 5),
   CALLDEF(ema_last, 5),
   CALLDEF(ema_linear, 5),
   
   // SMA functions
   CALLDEF(sma_next, 6),
   CALLDEF(sma_last, 6),
   CALLDEF(sma_linear, 6),
   
   // Rolling functions
   CALLDEF(rolling_central_moment, 7),
   CALLDEF(rolling_max, 6),
   CALLDEF(rolling_mean, 6),
   CALLDEF(rolling_median, 6),
   CALLDEF(rolling_min, 6),
   CALLDEF(rolling_num_obs, 6),
   CALLDEF(rolling_product, 6),
   CALLDEF(rolling_sd, 6),
   CALLDEF(rolling_sum, 6),
   CALLDEF(rolling_sum_stable, 6),
   CALLDEF(rolling_var, 6),
   {NULL, NULL, 0}
};


void R_init_utsOperators(DllInfo *info)
{
  R_registerRoutines(info, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
  R_forceSymbols(info, TRUE);
}