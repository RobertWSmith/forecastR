library(testthat)
context("Testing tbats `models.R`.")

test_that("`tbats` function", {
  a <- forecastR::tbats(AirPassengers)
  aa <- forecast::tbats(AirPassengers)

  a.mdl <- model(a)

  expect_is(a.mdl, class(aa))
  expect_equal(start(a.mdl$y), start(AirPassengers))
  expect_equal(length(a.mdl$y), length(AirPassengers))
  expect_equal(frequency(a.mdl$y), frequency(AirPassengers))
  expect_equal(start(resid(a.mdl)), start(AirPassengers))
  expect_equal(length(resid(a.mdl)), length(AirPassengers))
  expect_equal(frequency(resid(a.mdl)), frequency(AirPassengers))
  expect_equal(start(fitted(a.mdl)), start(AirPassengers))
  expect_equal(length(fitted(a.mdl)), length(AirPassengers))
  expect_equal(frequency(fitted(a.mdl)), frequency(AirPassengers))
})
