context("sma")

test_that("argument checking and trivial cases work",{
  # Argument checking
  expect_error(sma(ex_uts(), 123))
  expect_error(sma(ex_uts(), ddays(1), type="abc"))
  expect_error(sma(ex_uts(), ddays(Inf)))
  
  # "uts" with <= 1 observations
  expect_identical(
    sma(uts(), ddays(1)),
    uts()
  )
  x <- uts(12.1, Sys.time())
  expect_identical(
    sma(x, ddays(1)),
    x
  )
  
  # zero-length time window
  expect_identical(
    sma(ex_uts(), ddays(0)),
    ex_uts()
  )
  
  # time series with only NAs
  x <- uts(as.numeric(c(NA, NA)), as.POSIXct("2015-06-06") + dhours(0:1))
  expect_identical(
    sma(x, ddays(1), NA_method="ignore"),
    x
  )
  expect_identical(
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
    sma(ex_uts(), ddays(-1), type="equal"),
    file="test-sma_equal_2.rds"
  )
})

test_that("sma_equal and sma_equal_R give the same result",{
  expect_equal(
    sma(ex_uts(), ddays(1), type="equal"),
    sma_equal_R(ex_uts(), ddays(1))
  )
  expect_equal(
    sma(ex_uts(), ddays(0), type="equal"),
    sma_equal_R(ex_uts(), ddays(0))
  )
  expect_equal(
    sma(ex_uts(), ddays(1000), type="equal"),
    sma_equal_R(ex_uts(), ddays(1000))
  )
})


test_that("sma_linear works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), type="linear"),
    file="test-sma_linear_1.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(-1), type="linear"),
    file="test-sma_linear_2.rds"
  )
})


test_that("sma_last works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), type="last"),
    file="test-sma_last_1.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(-1), type="last"),
    file="test-sma_last_2.rds"
  )
})

test_that("sma_last and sma_last_R give the same result",{
  expect_equal(
    sma(ex_uts(), ddays(1), type="last"),
    sma_last_R(ex_uts(), ddays(1))
  )
  expect_equal(
    sma(ex_uts(), ddays(0), type="last"),
    sma_last_R(ex_uts(), ddays(0))
  )
  expect_equal(
    sma(ex_uts(), ddays(1000), type="last"),
    sma_last_R(ex_uts(), ddays(1000))
  )
})



test_that("sma_next works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), type="next"),
    file="test-sma_next_1.rds"
  )
  expect_equal_to_reference(
    sma(ex_uts(), ddays(-1), type="next"),
    file="test-sma_next_2.rds"
  )
})


test_that("sma_next equal to sma_last with shifted observations",{
  # Define common parameters
  x <- ex_uts()
  tau <- ddays(1)
  
  # Calculate SMA last with shifted observations
  x_shifted <- uts(values = c(x$values, last(x)),
            times = c(start(x) - days(1), x$times))
  out <- sma(x_shifted, tau, type="last")
  res1 <- head(out, -1)
  
  # Cannot use expect_identical(), because using c() for "POSIXct" objects drops any "tzone" attribute
  expect_equal(
    res1,
    sma(x, tau, type="next")
  )
})

