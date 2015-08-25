context("sma")

test_that("argument checking and trivial cases work",{
  # Argument checking
  expect_error(sma(ex_uts(), 123))
  expect_error(sma("abc"))
  
  # "uts" with <= 1 observations
  expect_equal(
    sma(uts(), ddays(1)),
    uts()
  )
  x <- uts(12.1, Sys.time())
  expect_equal(
    sma(x, ddays(1)),
    x
  )
  
  # zero-length time window
  expect_equal(
    sma(ex_uts(), ddays(0)),
    ex_uts()
  )
})


test_that("sma_equal works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), tye="equal"),
    file="test-sma_equal_1.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(Inf), tye="equal"),
    file="test-sma_equal_2.rds"
  )
})
