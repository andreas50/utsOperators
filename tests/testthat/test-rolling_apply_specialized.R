context("rolling_apply_specialized")

test_that("have_rolling_apply_specialized works",{
  expect_true(have_rolling_apply_specialized(ex_uts(), FUN=mean))
  expect_true(have_rolling_apply_specialized(ex_uts(), FUN="mean"))
  FUN <- mean
  expect_true(have_rolling_apply_specialized(ex_uts(), FUN=FUN))
  
  expect_false(have_rolling_apply_specialized(ex_uts(), FUN=mean, by=ddays(1)))
  expect_false(have_rolling_apply_specialized(uts(NA, Sys.time()), FUN=mean))
  expect_false(have_rolling_apply_specialized(uts(Inf, Sys.time()), FUN=mean))
})


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
    rolling_apply(ex_uts(), ddays(1), FUN=length),
    rolling_apply(ex_uts(), ddays(1), FUN=length, use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=length, align="center"),
    rolling_apply(ex_uts(), ddays(1), FUN=length, align="center", use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=length, align="left"),
    rolling_apply(ex_uts(), ddays(1), FUN=length, align="left", use_specialized=FALSE)
  )
  
  # FUN = min
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=min),
    rolling_apply(ex_uts(), ddays(1), FUN=min, use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=min, align="center"),
    rolling_apply(ex_uts(), ddays(1), FUN=min, align="center", use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=min, align="left"),
    rolling_apply(ex_uts(), ddays(1), FUN=min, align="left", use_specialized=FALSE)
  )
  
  # FUN = max
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=max),
    rolling_apply(ex_uts(), ddays(1), FUN=max, use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=max, align="center"),
    rolling_apply(ex_uts(), ddays(1), FUN=max, align="center", use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=max, align="left"),
    rolling_apply(ex_uts(), ddays(1), FUN=max, align="left", use_specialized=FALSE)
  )
  
  # FUN = mean
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=mean),
    rolling_apply(ex_uts(), ddays(1), FUN=mean, use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=mean, align="center"),
    rolling_apply(ex_uts(), ddays(1), FUN=mean, align="center", use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=mean, align="left"),
    rolling_apply(ex_uts(), ddays(1), FUN=mean, align="left", use_specialized=FALSE)
  )
  
  # FUN = median
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=median),
    rolling_apply(ex_uts(), ddays(1), FUN=median, use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=median, align="center"),
    rolling_apply(ex_uts(), ddays(1), FUN=median, align="center", use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=median, align="left"),
    rolling_apply(ex_uts(), ddays(1), FUN=median, align="left", use_specialized=FALSE)
  )
  
  # FUN = sum
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=sum),
    rolling_apply(ex_uts(), ddays(1), FUN=sum, use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=sum, align="center"),
    rolling_apply(ex_uts(), ddays(1), FUN=sum, align="center", use_specialized=FALSE)
  )
  expect_equal(
    rolling_apply(ex_uts(), ddays(1), FUN=sum, align="left"),
    rolling_apply(ex_uts(), ddays(1), FUN=sum, align="left", use_specialized=FALSE)
  )
})



