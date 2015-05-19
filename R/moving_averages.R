#############################################
# SMAs, EMAs, and similar rolling functions #
#############################################

# Define generic functions
sma <- function(x, ...) UseMethod("sma")
ema <- function(x, ...) UseMethod("ema")
