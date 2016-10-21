context("Testing durbin.watson.test `autocorrelation.tests.R`.")

test_that("`durbin.watson.test` function", {

  data("AirPassengers", package="datasets")

  aa <- arima(AirPassengers)
  #class = c("act", "DurbinWatsonTest")
  dwt <- durbin.watson.test(aa)
  dwt2 <- durbin.watson.test(resid(aa))

  expect_is(dwt, "act")
  expect_is(dwt, "DurbinWatsonTest")
  expect_is(dwt2, "act")
  expect_is(dwt2, "DurbinWatsonTest")

  expect_equal(dwt$is.significant, dwt2$is.significant)
  expect_equal(dwt$squared, dwt2$squared)

  ## testing GARCH residual checks
  sq <- durbin.watson.test(aa, squared=TRUE)
  sq2 <- durbin.watson.test(resid(aa), squared=TRUE)
  #transformation externally applied
  sq3 <- durbin.watson.test(resid(aa)^2, squared=FALSE)

  expect_equal(sq$is.significant, sq2$is.significant)
  expect_equal(sq$is.significant, sq3$is.significant)
  expect_equal(sq$squared, sq2$squared)

  #squared set to false in `sq3` because transformation was externally applied
  expect_equal(sq$squared, !sq3$squared)
})
