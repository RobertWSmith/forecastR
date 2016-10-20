context("Testing `models.R`.")

test_that("`arima` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
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
})

test_that("`sarima` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::sarima(AirPassengers)
  aa <- forecast::auto.arima(AirPassengers, D=1)

  a.mdl <- model(a)
  expect_is(a.mdl, class(aa))

  expect_equal(start(a.mdl$x), start(AirPassengers))
  expect_equal(length(a.mdl$x), length(AirPassengers))
  expect_equal(frequency(a.mdl$x), frequency(AirPassengers))

  expect_equal(start(resid(a.mdl)), start(AirPassengers))
  expect_equal(length(resid(a.mdl)), length(AirPassengers))
  expect_equal(frequency(resid(a.mdl)), frequency(AirPassengers))
})

test_that("`arfima` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::arfima(AirPassengers)
  aa <- forecast::arfima(AirPassengers)

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

test_that("`bats` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::bats(AirPassengers)
  aa <- forecast::bats(AirPassengers)

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

test_that("`ets` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::ets(AirPassengers)
  aa <- forecast::ets(AirPassengers)

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

test_that("`ets.multiplicative` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::ets.multiplicative(AirPassengers)
  aa <- forecast::ets(AirPassengers, allow.multiplicative.trend = TRUE)

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

test_that("`nnetar` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::nnetar(AirPassengers)
  aa <- forecast::nnetar(AirPassengers)

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

test_that("`nnetar.w.decay` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::nnetar(AirPassengers)
  aa <- forecast::nnetar(AirPassengers, decay=0.5)

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

test_that("`tbats` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
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

test_that("`forecast.tsm` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::arima(AirPassengers)
  aa <- forecast::auto.arima(AirPassengers)
  fa <- forecastR::forecast(a)
  faa <- forecast::forecast(aa)
  expect_is(fa, class(faa))
})
