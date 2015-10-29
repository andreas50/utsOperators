context("rolling_apply")

test_that("rolling_time_window works",{
  # Argument checking
  expect_error(rolling_time_window("2000-01-01", "2001-01-01", width=5, by=-ddays(3)))
  expect_error(rolling_time_window("2000-01-01", "2001-01-01", width=ddays(-5), by=ddays(-3)))
  expect_error(rolling_time_window("2000-01-01", "2001-01-01", width=ddays(5), by=3))
  expect_error(rolling_time_window("2000-01-01", "2001-01-01", width=ddays(5), by=ddays(-3)))
  expect_error(rolling_time_window("2010-01-01", "2001-01-01", width=ddays(5), by=ddays(3)))
  
  # Regression tests
  expect_equal_to_reference(
    rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30)),
    file="test-rolling_time_window"
  )
})


test_that("rolling_apply_static works",{
  start <- seq(as.POSIXct("2007-11-08"), as.POSIXct("2007-11-09 12:00:00"), by="12 hours")
  end <- start + dhours(8)
  
  # Argument checking
  expect_error(rolling_apply_static("abc"))
  expect_error(rolling_apply_static(ex_uts(), "abc", end, FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), start, "abc", FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), end, start, FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), start, c(end, Sys.time()), FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), rev(start), end, FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), rev(start), rev(end), FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), start, end, FUN=mean, align="abc"))
  expect_error(rolling_apply_static(ex_uts2(), start, end, FUN=mean))
  
  # Trivial case of no window
  expect_equal(
    rolling_apply_static(ex_uts(), as.POSIXct(character()), as.POSIXct(character()), FUN=mean),
    uts()
  )
  
  # Regression tests
  expect_equal_to_reference(
    rolling_apply_static(ex_uts(), start, end, FUN=mean),
    file="test-rolling_apply_static_1"
  )
  expect_equal_to_reference(
    rolling_apply_static(ex_uts(), start, end, FUN=mean, interior=TRUE),
    file="test-rolling_apply_static_2"
  )
})


test_that("rolling_apply works",{
  # Argument checking
  expect_error(rolling_apply(ex_uts(), width="abc"))
  expect_error(rolling_apply(ex_uts(), width=ddays(-1)))
  expect_error(rolling_apply(ex_uts(), width=ddays(1), by="abc"))
  expect_error(rolling_apply(ex_uts(), width=ddays(1), by=ddays(-1)))
  expect_error(rolling_apply(ex_uts(), width=ddays(1), by=ddays(1), align="abc"))
  expect_error(rolling_apply(ex_uts(), width=ddays(Inf), by=ddays(1)))
  
  # Regression tests
  expect_equal_to_reference(
    rolling_apply(ex_uts(), width=ddays(0.1), FUN="mean", by=ddays(0.1)),
    file="test-rolling_apply_1"
  )
  expect_equal_to_reference(
    rolling_apply(ex_uts(), width=ddays(0.1), FUN="mean", by=ddays(0.1), interior=TRUE),
    file="test-rolling_apply_2"
  )
  expect_equal_to_reference(
    rolling_apply(ex_uts(), width=ddays(1), FUN="mean"),
    file="test-rolling_apply_3"
  )
  expect_equal_to_reference(
    rolling_apply(ex_uts(), width=ddays(1), FUN="mean", interior=TRUE),
    file="test-rolling_apply_4"
  )
})

