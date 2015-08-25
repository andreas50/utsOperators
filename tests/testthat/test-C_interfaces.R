context("C interfaces")

test_that("generic_C_interface works",{
  # Argument checking
  expect_error(generic_C_interface(ex_uts2()))
  expect_error(generic_C_interface("abc"))
  
  # Empty "uts"
  expect_equal(
    generic_C_interface(uts()),
    uts()
  )
  
  # Prepare sample "uts" for regression tests
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


test_that("generic_C_interface_rolling works",{
  # Argument checking
  expect_error(generic_C_interface_rolling(uts(), tau=5))
  expect_error(generic_C_interface_rolling(uts(), tau=as.duration(NA)))
})


test_that("rev works",{
  expect_equal(
    rev(uts()),
    uts()
  )
  expect_equal(
    rev(rev(ex_uts())),
    ex_uts()
  )
  expect_equal(
    rev(ex_uts())$values,
    rev(ex_uts()$values)
  )
  expect_equal(
    rev(diff(ex_uts()$times)),
    diff(rev(ex_uts())$times)
  )
})