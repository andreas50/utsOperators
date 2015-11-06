context("rolling_apply_specialized")

test_that("rolling_apply_specialized argument checking",{
  # width
  expect_error(rolling_apply_specialized(ex_uts(), width="abc", FUN=sum))
  expect_error(rolling_apply_specialized(ex_uts(), width=ddays(0), FUN=sum))
  expect_error(rolling_apply_specialized(ex_uts(), width=ddays(-1), FUN=sum))
  expect_error(rolling_apply_specialized(ex_uts(), width=ddays(Inf), FUN=sum))
  
  # FUN argument
  expect_error(rolling_apply_specialized(ex_uts(), ddays(1), FUN="abc"))
  expect_error(rolling_apply_specialized(ex_uts(), ddays(1), FUN=function(x) + 1))
})


test_that("rolling_apply_specialized gives the same results as rolling_apply",{
  # FUN = length
  expect_equal(
    rolling_apply_specialized(ex_uts(), ddays(1), FUN=length),
    rolling_apply(ex_uts(), width=ddays(1), FUN=length)
  )
  
  # FUN = min
  expect_equal(
    rolling_apply_specialized(ex_uts(), ddays(1), FUN=min),
    rolling_apply(ex_uts(), width=ddays(1), FUN=min)
  )
  
  # FUN = max
  expect_equal(
    rolling_apply_specialized(ex_uts(), ddays(1), FUN=max),
    rolling_apply(ex_uts(), width=ddays(1), FUN=max)
  )
  
  # FUN = sum
  expect_equal(
    rolling_apply_specialized(ex_uts(), ddays(1), FUN=sum),
    rolling_apply(ex_uts(), width=ddays(1), FUN=sum)
  )
})


