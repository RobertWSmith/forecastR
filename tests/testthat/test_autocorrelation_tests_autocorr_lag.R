library(testthat)
context("Testing autocorr.lag `autocorrelation.tests.R`.")

test_that("`autocorr.lag` function", {
  freq <- frequency(AirPassengers)
  len <- length(AirPassengers)
  al <- autocorr.lags(AirPassengers)

  expect_equal(al, (min((2 * freq), ceiling(len/5), na.rm = TRUE)))
})
