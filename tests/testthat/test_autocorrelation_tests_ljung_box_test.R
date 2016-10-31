library(testthat)
context("Testing ljung.box.test `autocorrelation.tests.R`.")

test_that("`ljung.box.test` function", {
  aa <- arima(AirPassengers)

  lbt <- ljung.box.test(aa)
  lbt2 <- ljung.box.test(resid(aa))

  expect_is(lbt, "act")
  expect_is(lbt, "LjungBoxTest")
  expect_is(lbt2, "act")
  expect_is(lbt2, "LjungBoxTest")

  expect_equal(lbt$is.significant, lbt2$is.significant)
})
