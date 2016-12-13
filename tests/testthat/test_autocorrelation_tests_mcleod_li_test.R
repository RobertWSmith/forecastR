library(testthat)
context("Testing mcleod.li.test `autocorrelation.tests.R`.")

test_that("`mcleod.li.test` function", {
  aa <- arima(AirPassengers)

  mlt <- mcleod.li.test(aa)
  mlt2 <- mcleod.li.test(residuals(aa))

  expect_is(mlt, "act")
  expect_is(mlt, "McLeodLiTest")
  expect_true(mlt$is.significant)

  expect_is(mlt2, "act")
  expect_is(mlt2, "McLeodLiTest")
  expect_true(mlt2$is.significant)

  e <- ets(AirPassengers)
  mlt <- mcleod.li.test(residuals(e))

  expect_is(mlt, "act")
  expect_is(mlt, "McLeodLiTest")

  expect_false(mlt$is.significant)
})
