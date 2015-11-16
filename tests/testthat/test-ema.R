context("ema")

test_that("argument checking and trivial cases work",{
  # Argument checking
  expect_error(ema(ex_uts(), 123))
  expect_error(ema(ex_uts()))
  expect_error(ema(ex_uts(), ddays(1), interpolation="abc"))
  expect_error(ema(ex_uts(), ddays(Inf)))
  
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


test_that("an extremely long EMA gives a flat output",{
  x <- ex_uts()
  tau <- ddays(1e20)
  exptected_ema_values <- rep(first(x), length(x))
  
  expect_equal(
    ema(x, tau, interpolation="last")$values,
    exptected_ema_values <- rep(first(x), length(x))
  )
  expect_equal(
    ema(x, tau, interpolation="next")$values,
    exptected_ema_values <- rep(first(x), length(x))
  )
  expect_equal(
    ema(x, tau, interpolation="linear")$values,
    exptected_ema_values <- rep(first(x), length(x))
  )
})



### EMA_linear ###

test_that("ema_linear works",{
  # Regressions tests
  expect_equal_to_reference(
    ema(ex_uts(), ddays(1), interpolation="linear"),
    file="test-ema_linear_1.rds"
  )
  expect_equal_to_reference(
    ema(ex_uts(), ddays(-1), interpolation="linear"),
    file="test-ema_linear_2.rds"
  )
})



### EMA_last ###

test_that("ema_last works",{
  # Regressions tests
  expect_equal_to_reference(
    ema(ex_uts(), ddays(1), interpolation="last"),
    file="test-ema_last_1.rds"
  )
  expect_equal_to_reference(
    ema(ex_uts(), ddays(-1), interpolation="last"),
    file="test-ema_last_2.rds"
  )
})



### EMA_next ###

test_that("ema_next works",{
  # Regressions tests
  expect_equal_to_reference(
    ema(ex_uts(), ddays(1), interpolation="next"),
    file="test-ema_next_1.rds"
  )
  expect_equal_to_reference(
    ema(ex_uts(), ddays(-1), interpolation="next"),
    file="test-ema_next_2.rds"
  )
})


