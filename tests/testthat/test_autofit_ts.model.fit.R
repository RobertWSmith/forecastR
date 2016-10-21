context("Testing ts.model.fit `autofit.R`.")

test_that("`ts.model.fit` & `ts.model.refit` functions", {
  library(forecast)
  data("AirPassengers", package="datasets")
  ap <- ts.split(AirPassengers, 24)

  model.types <- c("arima", "arfima", "bats", "ets", "nnetar", "stlm", "tbats", "tslm")

  mdl.refit <- mdl.fit <- list()
  # check all models fit data against default arguments
  for (func in model.types)
  {
    tmp.mdl <- ts.model.fit(ap$in.sample, ts.model.type = func)
    tmp.refit <- ts.model.refit(ap$data, model = tmp.mdl)

    mdl.fit[[func]] <- tmp.mdl
    mdl.refit[[func]] <- tmp.refit

    ## testing that refit model has equivalent coefficients, with the exception
    ## of ARFIMA models, which do not currently support being refit
    if (!(func %in% c("arfima", "stlm")))
    {
      nms <- names(tmp.mdl)
      expect_equal(coef(tmp.mdl)[nms], coef(tmp.refit)[nms], info=func)
    }
    # cleanup to ensure not retesting against old values inside loop
    tmp.mdl <- tmp.refit <- NULL
  }

  # no NULL were returned
  expect_equal(length(mdl.fit), length(model.types))

  ## validate exact same model is returned by directly calling model functions
  aa <- arima(ap$in.sample)
  expect_equal(coef(mdl.fit[['arima']]), coef(aa))
  expect_equal(coef(mdl.refit[['arima']]), coef(aa))

  af <- arfima(ap$in.sample)
  expect_equal(coef(mdl.fit[['arfima']]), coef(af))
  expect_equal(coef(mdl.refit[['arfima']]), coef(af))

  ba <- bats(ap$in.sample)
  expect_equal(coef(mdl.fit[['bats']]), coef(ba))
  expect_equal(coef(mdl.refit[['bats']]), coef(ba))

  et <- ets(ap$in.sample)
  expect_equal(coef(mdl.fit[['ets']]), coef(et))
  expect_equal(coef(mdl.refit[['ets']]), coef(et))

  nn <- nnetar(ap$in.sample)
  expect_equal(coef(mdl.fit[['nnetar']]), coef(nn))
  expect_equal(coef(mdl.refit[['nnetar']]), coef(nn))

  st <- stlm(ap$in.sample)
  expect_equal(coef(mdl.fit[['stlm']]), coef(st))
  expect_equal(coef(mdl.refit[['stlm']]), coef(st))

  tb <- tbats(ap$in.sample)
  expect_equal(coef(mdl.fit[['tbats']]), coef(tb))
  expect_equal(coef(mdl.refit[['tbats']]), coef(tb))

  sl <- tslm(ap$in.sample)
  expect_equal(coef(mdl.fit[['tslm']]), coef(sl))
  expect_equal(coef(mdl.refit[['tslm']]), coef(sl))
})
