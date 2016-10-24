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
    if (!(func %in% c("arfima", "stlm", "tslm")))
    {
      expect_equal(length(coef(tmp.mdl)), length(coef(tmp.refit)), info=func)
    }
    # cleanup to ensure not retesting against old values inside loop
    tmp.mdl <- tmp.refit <- NULL
  }

  # no NULL were returned
  expect_equal(length(mdl.fit), length(model.types))

  ## validate exact same model is returned by directly calling model functions
  aa <- arima(ap$in.sample)
  expect_equal(length(coef(mdl.fit[['arima']])), length(coef(aa)))
  expect_equal(length(coef(mdl.refit[['arima']])), length(coef(aa)))

  ba <- bats(ap$in.sample)
  expect_equal(length(coef(mdl.fit[['bats']])), length(coef(ba)))
  expect_equal(length(coef(mdl.refit[['bats']])), length(coef(ba)))

  et <- ets(ap$in.sample)
  expect_equal(length(coef(mdl.fit[['ets']])), length(coef(et)))
  expect_equal(length(coef(mdl.refit[['ets']])), length(coef(et)))

  nn <- nnetar(ap$in.sample)
  expect_equal(length(coef(mdl.fit[['nnetar']])), length(coef(nn)))
  expect_equal(length(coef(mdl.refit[['nnetar']])), length(coef(nn)))

  tb <- tbats(ap$in.sample)
  expect_equal(length(coef(mdl.fit[['tbats']])), length(coef(tb)))
  expect_equal(length(coef(mdl.refit[['tbats']])), length(coef(tb)))
})
