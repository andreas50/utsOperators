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
    file="test-rolling_time_window_1.rds"
  )
  expect_equal_to_reference(
    rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30), interior=TRUE),
    file="test-rolling_time_window_2.rds"
  )
})


test_that("rolling_time_window_indices works",{
  tmp <- rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30))
  start_times <- tmp$start_times
  end_times <- tmp$end_times
  times <- seq(as.POSIXct("2014-12-01"), as.POSIXct("2015-12-30"), by="week")
  
  # Argument checking
  expect_error(rolling_time_window_indices("abc", times, end_times))
  expect_error(rolling_time_window_indices(times, "abc", end_times))
  expect_error(rolling_time_window_indices(times, start_times, "abc"))
  expect_error(rolling_time_window_indices(times, end_times, start_times))
  expect_error(rolling_time_window_indices(times, start_times, c(end_times, Sys.time())))
  expect_error(rolling_time_window_indices(times, rev(start_times), end_times))
  expect_error(rolling_time_window_indices(times, start_times, rev(end_times)))
  
  # Trivial case of zero-length time windows
  expect_identical(
    rolling_time_window_indices(times, times, times)$start_index - 1L,
    rolling_time_window_indices(times, times, times)$end_index
  )
  
  # Windows contain no observations
  expect_identical(
    rolling_time_window_indices(times, times + dhours(1), times + dhours(2))$start_index,
    2L:(length(times) + 1)
  )
  expect_identical(
    rolling_time_window_indices(times, times + dhours(1), times + dhours(2))$end_index,
    1:length(times)
  )
  
  # Trivial case of one observation in each time window
  expect_identical(
    rolling_time_window_indices(times[-1], times[-length(times)], times[-1])$start_index,
    rolling_time_window_indices(times[-1], times[-length(times)], times[-1])$end_index
  )
  
  # Regression tests
  expect_equal_to_reference(
    rolling_time_window_indices(times, start_times, end_times),
    file="test-rolling_time_window_indices.rds"
  )
})


test_that("rolling_apply_static works",{
  start_times <- seq(as.POSIXct("2007-11-08"), as.POSIXct("2007-11-09 12:00:00"), by="12 hours")
  end_times <- start_times + dhours(8)
  
  # Argument checking
  expect_error(rolling_apply_static("abc"))
  expect_error(rolling_apply_static(ex_uts(), "abc", end_times, FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), start_times, "abc", FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), end_times, start_times, FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), start_times, c(end_times, Sys.time()), FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), rev(start_times), end_times, FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), rev(start_times), rev(end_times), FUN=mean))
  expect_error(rolling_apply_static(ex_uts(), start_times, end_times, FUN=mean, align="abc"))
  expect_error(rolling_apply_static(ex_uts2(), start_times, end_times, FUN=mean))
  
  # Trivial case of no window
  expect_identical(
    rolling_apply_static(ex_uts(), as.POSIXct(character()), as.POSIXct(character()), FUN=mean),
    uts()
  )
  
  # Regression tests
  expect_equal_to_reference(
    rolling_apply_static(ex_uts(), start_times, end_times, FUN=mean),
    file="test-rolling_apply_static_1.rds"
  )
  expect_equal_to_reference(
    rolling_apply_static(ex_uts(), start_times, end_times, FUN=mean, interior=TRUE),
    file="test-rolling_apply_static_2.rds"
  )
})


test_that("rolling_apply works",{
  # Argument checking
  expect_error(rolling_apply(ex_uts(), width="abc"))
  expect_error(rolling_apply(ex_uts(), width=ddays(0)))
  expect_error(rolling_apply(ex_uts(), width=ddays(-1)))
  expect_error(rolling_apply(ex_uts(), width=ddays(1), by="abc"))
  expect_error(rolling_apply(ex_uts(), width=ddays(1), by=ddays(-1)))
  expect_error(rolling_apply(ex_uts(), width=ddays(1), by=ddays(1), align="abc"))
  expect_error(rolling_apply(ex_uts(), width=ddays(Inf), by=ddays(1)))
  
  # Regression tests
  expect_equal_to_reference(
    rolling_apply(ex_uts(), width=ddays(0.1), FUN="mean", by=ddays(0.1), use_specialized=FALSE),
    file="test-rolling_apply_1.rds"
  )
  expect_equal_to_reference(
    rolling_apply(ex_uts(), width=ddays(0.1), FUN="mean", by=ddays(0.1), interior=TRUE, use_specialized=FALSE),
    file="test-rolling_apply_2.rds"
  )
  expect_equal_to_reference(
    rolling_apply(ex_uts(), width=ddays(1), FUN="mean", use_specialized=FALSE),
    file="test-rolling_apply_3.rds"
  )
  expect_equal_to_reference(
    rolling_apply(ex_uts(), width=ddays(1), FUN="mean", interior=TRUE, use_specialized=FALSE),
    file="test-rolling_apply_4.rds"
  )
})

