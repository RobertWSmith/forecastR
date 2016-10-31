library(testthat)
context("Testing `forecast.R`.")

test_that("`forecast` arima function", {
  a <- forecastR::arima(AirPassengers)
  fa <- forecastR::forecast(a)

  aa <- forecast::auto.arima(AirPassengers)
  faa <- forecast::forecast(aa)

  expect_is(fa, class(faa))
})


test_that("`forecast` arfima function", {
  a <- forecastR::arfima(AirPassengers)
  fa <- forecastR::forecast(a)

  aa <- forecast::arfima(AirPassengers)
  faa <- forecast::forecast(aa)

  expect_is(fa, class(faa))
})


test_that("`forecast` bats function", {
  a <- forecastR::bats(AirPassengers)
  fa <- forecastR::forecast(a)

  aa <- forecast::bats(AirPassengers)
  faa <- forecast::forecast(aa)

  expect_is(fa, class(faa))
})


test_that("`forecast` ets function", {
  a <- forecastR::ets(AirPassengers)
  fa <- forecastR::forecast(a)

  aa <- forecast::ets(AirPassengers)
  faa <- forecast::forecast(aa)

  expect_is(fa, class(faa))
})


test_that("`forecast` nnetar function", {
  a <- forecastR::nnetar(AirPassengers)
  fa <- forecastR::forecast(a)

  aa <- forecast::nnetar(AirPassengers)
  faa <- forecast::forecast(aa)

  expect_is(fa, class(faa))
})

test_that("`forecast` stlm function", {
  a <- forecastR::stlm(AirPassengers)
  fa <- forecastR::forecast(a)

  aa <- forecast::stlm(AirPassengers)
  faa <- forecast::forecast(aa)

  expect_is(fa, class(faa))
})


test_that("`forecast` tbats function", {
  a <- forecastR::tbats(AirPassengers)
  fa <- forecastR::forecast(a)

  aa <- forecast::tbats(AirPassengers)
  faa <- forecast::forecast(aa)

  expect_is(fa, class(faa))
})

test_that("`forecast` tslm function", {
  a <- forecastR::tslm(AirPassengers)
  fa <- forecastR::forecast(a)

  aa <- forecast::tslm(AirPassengers ~ trend + season)
  faa <- forecast::forecast(aa)

  expect_is(fa, class(faa))
})

test_that("`multiforecast` function", {
  ### ensure matrix is filled in appropriately
  ### fixes 2016-10-24 issue where forecasts were combined in matrix which
  ### did not have rows aligned by start/end/frequency

  mmf <- ts.multimodel.fit(AirPassengers)
  mfcst <- multiforecast(mmf, y=AirPassengers)
  expect_true(all(complete.cases(mfcst)))

})
