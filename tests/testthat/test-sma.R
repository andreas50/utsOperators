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
  
  # time series with only NAs
  x <- uts(as.numeric(c(NA, NA)), as.POSIXct("2015-06-06") + dhours(0:1))
  expect_equal(
    sma(x, ddays(1), NA_method="ignore"),
    x
  )
  expect_equal(
    sma(x, ddays(1), NA_method="omit"),
    uts()
  )
  expect_error(sma(x, ddays(1), NA_method="fail"))
})


test_that("sma_equal works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), type="equal"),
    file="test-sma_equal_1.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(Inf), type="equal"),
    file="test-sma_equal_2.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(-1), type="equal"),
    file="test-sma_equal_3.rds"
  )
})


test_that("sma_linear works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), type="linear"),
    file="test-sma_linear_1.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(Inf), type="linear"),
    file="test-sma_linear_2.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(-1), type="linear"),
    file="test-sma_linear_3.rds"
  )
})


test_that("sma_last works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), type="last"),
    file="test-sma_last_1.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(Inf), type="last"),
    file="test-sma_last_2.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(-1), type="last"),
    file="test-sma_last_3.rds"
  )
})



test_that("sma_next works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), type="next"),
    file="test-sma_next_1.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(Inf), type="next"),
    file="test-sma_next_2.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(-1), type="next"),
    file="test-sma_next_3.rds"
  )
})
