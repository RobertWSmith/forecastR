library(testthat)
context("Testing arima `models.R`.")

test_that("base `arima`", {
  a <- forecastR::arima(AirPassengers)
  aa <- forecast::auto.arima(AirPassengers)

  a.mdl <- model(a)

  expect_is(a.mdl, class(aa))
  expect_equal(start(a.mdl$x), start(AirPassengers))
  expect_equal(length(a.mdl$x), length(AirPassengers))
  expect_equal(frequency(a.mdl$x), frequency(AirPassengers))
  expect_equal(start(resid(a.mdl)), start(AirPassengers))
  expect_equal(length(resid(a.mdl)), length(AirPassengers))
  expect_equal(frequency(resid(a.mdl)), frequency(AirPassengers))

  a <- forecastR::arima(AirPassengers, D = 1)
  aa <- forecast::auto.arima(AirPassengers, D = 1)

  a.mdl <- model(a)

  expect_is(a.mdl, class(aa))
  expect_equal(start(a.mdl$x), start(AirPassengers))
  expect_equal(length(a.mdl$x), length(AirPassengers))
  expect_equal(frequency(a.mdl$x), frequency(AirPassengers))
  expect_equal(start(resid(a.mdl)), start(AirPassengers))
  expect_equal(length(resid(a.mdl)), length(AirPassengers))
  expect_equal(frequency(resid(a.mdl)), frequency(AirPassengers))
})
