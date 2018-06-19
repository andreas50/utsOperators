context("C interfaces")

test_that("generic_C_interface works",{
  # Argument checking
  expect_error(generic_C_interface(ex_uts2(), "sma_last"))
  expect_error(generic_C_interface("abc", "sma_last"))
  
  # Time series with unequal number of observation values and times
  x <- ex_uts()
  x$values <- c(x$values, 0)
  expect_error(generic_C_interface(x, "sma_last"))
  
  # NA argument checking
  x <- ex_uts()
  x$values[2] <- NA
  expect_error(generic_C_interface(x, "sma_last"))
  
  # Finite value argument checking
  x <- ex_uts()
  x$values[2] <- Inf
  expect_error(generic_C_interface(x, "sma_last"))
  
  # Empty "uts"
  expect_identical(
    generic_C_interface(uts(), "sma_last", width_before=ddays(1), width_after=ddays(1)),
    uts()
  )
})

