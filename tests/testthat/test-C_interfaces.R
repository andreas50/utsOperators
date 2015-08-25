context("C utility functions")

test_that("generic_C_interface works",{
  expect_equal(
    generic_C_interface(uts()),
    uts()
  )
  
  # Prepare sample "uts"
  x <- ex_uts()
  x$values[2] <- NA
  
  # Regressions tests
  expect_equal_to_reference(
    generic_C_interface(x, "sma_equal", tau=ddays(1)),
    file="test-C_interface_1.rds"
  )
  expect_equal_to_reference(
    generic_C_interface(x, "sma_equal", tau=ddays(1), NA_method="omit"),
    file="test-C_interface_2.rds"
  )
  expect_equal_to_reference(
    generic_C_interface(x, "sma_last", tau=ddays(1)),
    file="test-C_interface_3.rds"
  )
  expect_equal_to_reference(
    generic_C_interface(x, "sma_last", tau=ddays(1), NA_method="omit"),
    file="test-C_interface_4.rds"
  )
})