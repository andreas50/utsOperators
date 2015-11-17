context("helper functions")

test_that("check_window_width works",{
  expect_error(check_window_width(5))
  expect_error(check_window_width(-ddays(5)))
  expect_error(check_window_width(Inf))
  expect_error(check_window_width(ddays(0)))
  
  expect_identical(
    check_window_width(ddays(0), require_positive=FALSE),
    NULL
  )
})


test_that("rev works",{
  expect_identical(
    rev(uts()),
    uts()
  )
  expect_identical(
    rev(rev(ex_uts())),
    ex_uts()
  )
  expect_identical(
    rev(ex_uts())$values,
    rev(ex_uts()$values)
  )
  expect_identical(
    rev(diff(ex_uts()$times)),
    diff(rev(ex_uts())$times)
  )
})