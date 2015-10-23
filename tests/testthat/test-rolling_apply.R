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
    rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30), align="right"),
    file="test-rolling_apply_1.rds"
  )
  expect_equal_to_reference(
    rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30), align="left"),
    file="test-rolling_apply_2.rds"
  )
  expect_equal_to_reference(
    rolling_time_window(start="2015-01-01", end="2015-06-30", width=ddays(90), by=ddays(30), align="center"),
    file="test-rolling_apply_3.rds"
  )
})