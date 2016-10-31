library(testthat)
context("Testing tslm `models.R`.")

test_that("`tslm` function", {
  a <- forecastR::tslm(AirPassengers)
  aa <- forecast::tslm(AirPassengers ~ trend + season)

  a.mdl <- model(a)

  expect_is(a.mdl, class(aa))
  expect_equal(start(resid(a.mdl)), start(AirPassengers))
  expect_equal(length(resid(a.mdl)), length(AirPassengers))
  expect_equal(frequency(resid(a.mdl)), frequency(AirPassengers))
  expect_equal(start(fitted(a.mdl)), start(AirPassengers))
  expect_equal(length(fitted(a.mdl)), length(AirPassengers))
  expect_equal(frequency(fitted(a.mdl)), frequency(AirPassengers))
})
