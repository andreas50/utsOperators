context("C interfaces")

test_that("generic_C_interface works",{
  # Argument checking
  expect_error(generic_C_interface(ex_uts2()))
  expect_error(generic_C_interface("abc"))
  
  # Empty "uts"
  expect_identical(
    generic_C_interface(uts(),"sma_last", width=ddays(1)),
    uts()
  )
  
  # Prepare sample "uts" for regression tests
  x <- ex_uts()
  x$values[2] <- NA
  
  # Regressions tests
  expect_equal_to_reference(
    generic_C_interface(x, "sma_last", width=ddays(1)),
    file="test-C_interface_1.rds"
  )
  expect_equal_to_reference(
    generic_C_interface(x, "sma_last", width=ddays(1), NA_method="omit"),
    file="test-C_interface_2.rds"
  )
  expect_equal_to_reference(
    generic_C_interface(x, "sma_last", width=ddays(1)),
    file="test-C_interface_3.rds"
  )
  expect_equal_to_reference(
    generic_C_interface(x, "sma_last", width=ddays(1), NA_method="omit"),
    file="test-C_interface_4.rds"
  )
})


test_that("generic_C_interface_rolling works",{
  # Argument checking
  expect_error(generic_C_interface_rolling(uts(), width=5))
  expect_error(generic_C_interface_rolling(uts(), width=as.duration(NA)))
  expect_error(generic_C_interface_rolling(uts(), width=ddays(-1)))
  expect_error(generic_C_interface_rolling(uts(), width=ddays(-Inf)))
})


test_that("rev works",{
  expect_identical(
    rev(uts()),
    uts()
  )
  expect_identical(
    rev(rev(ex_uts())),
    ex_uts()
  )
  expect_identical(
    rev(ex_uts())$values,
    rev(ex_uts()$values)
  )
  expect_identical(
    rev(diff(ex_uts()$times)),
    diff(rev(ex_uts())$times)
  )
})
