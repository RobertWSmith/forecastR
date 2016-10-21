context("Testing `forecast.R`.")

test_that("`forecast` function", {
  library(forecast)
  data("AirPassengers", package="datasets")
  a <- forecastR::arima(AirPassengers)
  aa <- forecast::auto.arima(AirPassengers)
  fa <- forecastR::forecast(a)
  faa <- forecast::forecast(aa)
  expect_is(fa, class(faa))
})
