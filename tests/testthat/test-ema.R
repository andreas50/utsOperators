context("ema")

test_that("argument checking and trivial cases work",{
  # Argument checking
  expect_error(ema(ex_uts(), 123))
  expect_error(ema(ex_uts()))
  expect_error(ema(ex_uts(), ddays(1), type="abc"))
  expect_error(ema("abc"))
  
  # "uts" with <= 1 observations
  expect_identical(
    ema(uts(), ddays(1)),
    uts()
  )
  x <- uts(12.1, Sys.time())
  expect_identical(
    ema(x, ddays(1)),
    x
  )
  
  # zero-length time window
  expect_identical(
    ema(ex_uts(), ddays(0)),
    ex_uts()
  )
  
  # time series with only NAs
  x <- uts(as.numeric(c(NA, NA)), as.POSIXct("2015-06-06") + dhours(0:1))
  expect_identical(
    ema(x, ddays(1), NA_method="ignore"),
    x
  )
  expect_identical(
    ema(x, ddays(1), NA_method="omit"),
    uts()
  )
  expect_error(ema(x, ddays(1), NA_method="fail"))
})


test_that("ema_linear works",{
  # Regressions tests
  expect_equal_to_reference(
    ema(ex_uts(), ddays(1), type="linear"),
    file="test-ema_linear_1.rds"
  )
  expect_equal_to_reference(
    ema(ex_uts(), ddays(-1), type="linear"),
    file="test-ema_linear_2.rds"
  )
})


test_that("ema_last works",{
  # Regressions tests
  expect_equal_to_reference(
    ema(ex_uts(), ddays(1), type="last"),
    file="test-ema_last_1.rds"
  )
  expect_equal_to_reference(
    ema(ex_uts(), ddays(-1), type="last"),
    file="test-ema_last_2.rds"
  )
})


test_that("ema_next works",{
  # Regressions tests
  expect_equal_to_reference(
    ema(ex_uts(), ddays(1), type="next"),
    file="test-ema_next_1.rds"
  )
  expect_equal_to_reference(
    ema(ex_uts(), ddays(-1), type="next"),
    file="test-ema_next_2.rds"
  )
})
