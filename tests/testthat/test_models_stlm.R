context("Testing stlm `models.R`.")


test_that("`stlm` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::stlm(AirPassengers)
  aa <- forecast::stlm(AirPassengers)

  a.mdl <- model(a)
  expect_is(a.mdl, class(aa))

  expect_equal(start(a.mdl$x), start(AirPassengers))
  expect_equal(length(a.mdl$x), length(AirPassengers))
  expect_equal(frequency(a.mdl$x), frequency(AirPassengers))

  expect_equal(start(resid(a.mdl)), start(AirPassengers))
  expect_equal(length(resid(a.mdl)), length(AirPassengers))
  expect_equal(frequency(resid(a.mdl)), frequency(AirPassengers))

  expect_equal(start(fitted(a.mdl)), start(AirPassengers))
  expect_equal(length(fitted(a.mdl)), length(AirPassengers))
  expect_equal(frequency(fitted(a.mdl)), frequency(AirPassengers))
})
