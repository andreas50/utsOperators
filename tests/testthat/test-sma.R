context("sma")

test_that("argument checking and trivial cases work",{
  # Argument checking
  expect_error(sma(ex_uts(), 123))
  expect_error(sma(ex_uts(), ddays(1), type="abc"))
  expect_error(sma(ex_uts(), ddays(0)))
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


test_that("an extremely long SMA gives a flat output for type 'last', 'next' and 'linear'",{
  x <- ex_uts()
  width <- ddays(1e20)
  exptected_sma_values <- rep(first(x), length(x))
  
  expect_equal(
    sma(x, width, type="last")$values,
    exptected_sma_values
  )
  expect_equal(
    sma(x, width, type="next")$values,
    exptected_sma_values
  )
  expect_equal(
    sma(x, width, type="linear")$values,
    exptected_sma_values
  )
})


### SMA_equal ###

test_that("sma_equal works",{
  # Very short time window
  expect_equal(
    sma(ex_uts(), dseconds(1), type="equal"),
    ex_uts()
  )
  
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
    sma(ex_uts(), dseconds(1), type="equal"),
    sma_equal_R(ex_uts(), dseconds(1))
  )
  expect_equal(
    sma(ex_uts(), ddays(1000), type="equal"),
    sma_equal_R(ex_uts(), ddays(1000))
  )
})

test_that("sma_equal and rolling_apply give the same result",{
  expect_equal(
    sma(ex_uts(), ddays(1), type="equal"),
    rolling_apply(ex_uts(), width=ddays(1), FUN="mean")
  )
})



### SMA_linear ###

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

test_that("sma_linear and sma_linear_R give the same result",{
  expect_equal(
    sma(ex_uts(), ddays(1), type="linear"),
    sma_linear_R(ex_uts(), ddays(1))
  )
  expect_equal(
    sma(ex_uts(), dseconds(1), type="linear"),
    sma_linear_R(ex_uts(), dseconds(1))
  )
  expect_equal(
    sma(ex_uts(), ddays(1000), type="linear"),
    sma_linear_R(ex_uts(), ddays(1000))
  )
})



### SMA_last ###

test_that("sma_equal special cases work",{
  # If the time window is shorter than the smallest observation time difference,
  # then SMA_last is equal to backshifted time series (apart from the first observation)
  x <- ex_uts()
  width <- as.duration(min(diff(x$times))) / 2
  expect_identical(
    head(sma(x, width, type="last"), -1),
    lag(x)
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
    sma(ex_uts(), dseconds(1), type="last"),
    sma_last_R(ex_uts(), dseconds(1))
  )
  expect_equal(
    sma(ex_uts(), ddays(1000), type="last"),
    sma_last_R(ex_uts(), ddays(1000))
  )
})



### SMA_next ###

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
  width <- ddays(1)
  
  # Calculate SMA last with shifted observations
  x_shifted <- uts(values = c(x$values, last(x)),
            times = c(start(x) - days(1), x$times))
  out <- sma(x_shifted, width, type="last")
  res1 <- head(out, -1)
  
  # Cannot use expect_identical(), because using c() for "POSIXct" objects drops any "tzone" attribute
  expect_equal(
    res1,
    sma(x, width, type="next")
  )
})

