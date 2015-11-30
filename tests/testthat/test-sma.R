context("sma")

test_that("argument checking and trivial cases work",{
  # Argument checking
  expect_error(sma(ex_uts(), 123))
  expect_error(sma(ex_uts(), ddays(1), interpolation="abc"))
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
})


test_that("an extremely long SMA gives a flat output",{
  x <- ex_uts()
  width <- ddays(1e20)
  exptected_sma_values <- rep(first(x), length(x))
  
  expect_equal(
    sma(x, width, interpolation="last")$values,
    exptected_sma_values
  )
  expect_equal(
    sma(x, width, interpolation="next")$values,
    exptected_sma_values
  )
  expect_equal(
    sma(x, width, interpolation="linear")$values,
    exptected_sma_values
  )
})


test_that("a flat uts gives a flat SMA",{
  x <- uts(rep(5, 10), as.POSIXct("2010-01-01") + ddays(1:10))

  # SMA_last
  expect_equal(
    sma(x, width=ddays(4), align="left", interpolation="last"),
    x
  )
  expect_equal(
    sma(x, width=ddays(4), align="right", interpolation="last"),
    x
  )
  expect_equal(
    sma(x, width=ddays(4), align="center", interpolation="last"),
    x
  )
  
  # SMA_next: coming soon
  
  # SMA_linear: coming soon
})



### SMA_linear ###

test_that("sma_linear works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), interpolation="linear"),
    file="test-sma_linear_1.rds"
  )
})

test_that("sma_linear and sma_linear_R give the same result",{
  expect_equal(
    sma(ex_uts(), ddays(1), interpolation="linear"),
    sma_linear_R(ex_uts(), ddays(1))
  )
  expect_equal(
    sma(ex_uts(), dseconds(1), interpolation="linear"),
    sma_linear_R(ex_uts(), dseconds(1))
  )
  expect_equal(
    sma(ex_uts(), ddays(1000), interpolation="linear"),
    sma_linear_R(ex_uts(), ddays(1000))
  )
})



### SMA_last ###

test_that("sma_last special cases work",{
  # If the time window is shorter than the smallest observation time difference,
  # then SMA_last is equal to backshifted time series (apart from the first observation)
  x <- ex_uts()
  width <- as.duration(min(diff(x$times))) / 2
  expect_identical(
    head(sma(x, width, interpolation="last"), -1),
    lag(x)
  )
})

test_that("sma_last works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), interpolation="last"),
    file="test-sma_last_1.rds"
  )
})

test_that("sma_last and sma_last_R give the same result",{
  expect_equal(
    sma(ex_uts(), ddays(1), interpolation="last"),
    sma_last_R(ex_uts(), ddays(1))
  )
  expect_equal(
    sma(ex_uts(), dseconds(1), interpolation="last"),
    sma_last_R(ex_uts(), dseconds(1))
  )
  expect_equal(
    sma(ex_uts(), ddays(1000), interpolation="last"),
    sma_last_R(ex_uts(), ddays(1000))
  )
})



### SMA_next ###

test_that("sma_next works",{
  # Regressions tests
  expect_equal_to_reference(
    sma(ex_uts(), ddays(1), interpolation="next"),
    file="test-sma_next_1.rds"
  )
})


test_that("sma_next equal to sma_last with shifted observations",{
  # Define common parameters
  x <- ex_uts()
  width <- ddays(1)
  
  # Calculate SMA last with shifted observations
  x_shifted <- uts(values = c(x$values, last(x)),
            times = c(start(x) - days(1), x$times))
  out <- sma(x_shifted, width, interpolation="last")
  res1 <- head(out, -1)
  
  # Cannot use expect_identical(), because using c() for "POSIXct" objects drops any "tzone" attribute
  expect_equal(
    res1,
    sma(x, width, interpolation="next")
  )
})

