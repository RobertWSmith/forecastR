context("Testing ts.model.fit `model_fit.R`.")

test_that("`ts.model.fit` & `ts.model.refit` functions", {
  library(forecast)
  data("AirPassengers", package = "datasets")
  ap <- ts.split(AirPassengers, 24)
  model.types <- c("arima", "arfima", "bats", "ets", "nnetar",
    "stlm", "tbats", "tslm")
  mdl.af <- mdl.refit <- mdl.fit <- list()

  # check all models fit data against default arguments
  for (func in model.types)
  {
    tmp.mdl <- ts.model.fit(ap$in.sample, ts.model.type = func)
    tmp.refit <- ts.model.refit(ap$data, model = tmp.mdl)
    tmp.af <- ts.model.autofit(ap$in.sample, return.all.models = TRUE)

    mdl.fit[[func]] <- tmp.mdl
    mdl.refit[[func]] <- tmp.refit
    mdl.af[[func]] <- tmp.af

    ## testing that refit model has equivalent coefficients, with the exception
    ## of ARFIMA models, which do not currently support being refit
    if (!(func %in% c("arfima", "stlm", "tslm")))
    {
      expect_equal(length(coef(tmp.mdl)), length(coef(tmp.refit)), info=func)
    }

    # for functions which allow explicit lambda, check that tmp.af has more than
    # one model
    if (!func %in% c("bats", "tbats"))
    {
      expect_gt(length(tmp.af), 1L)
    }

    # cleanup to ensure not retesting against old values inside
    # loop
    tmp.mdl <- tmp.refit <- tmp.af <- NULL
  }

  # no NULL were returned
  expect_equal(length(mdl.fit), length(model.types))

  ## validate exact same model is returned by directly calling
  ## model functions
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

  ap.short <- window(AirPassengers, end=start(AirPassengers)[1]+1.5)
  ap <- ts.split(ap.short)

  mdl.af <- mdl.refit <- mdl.fit <- list()

  ## "stlm" will always throw an error where ts length < 2x frequency
  model.types <- c("arima", "arfima", "bats", "ets", "nnetar", "tbats", "tslm")
  # check all models fit data against default arguments
  for (func in model.types)
  {
    tmp.mdl <- ts.model.fit(ap$in.sample, ts.model.type = func)
    tmp.refit <- ts.model.refit(ap$data, model = tmp.mdl)
    tmp.af <- ts.model.autofit(ap$in.sample, return.all.models = TRUE)

    mdl.fit[[func]] <- tmp.mdl
    mdl.refit[[func]] <- tmp.refit
    mdl.af[[func]] <- tmp.af

    ## testing that refit model has equivalent coefficients, with the exception
    ## of ARFIMA models, which do not currently support being refit
    if (!(func %in% c("arfima", "tslm")))
    {
      expect_equal(length(coef(tmp.mdl)), length(coef(tmp.refit)), info=func)
    }

    # for functions which allow explicit lambda, check that tmp.af has more than
    # one model
    if (!func %in% c("bats", "tbats"))
    {
      expect_gt(length(tmp.af), 1L)
    }

    # cleanup to ensure not retesting against old values inside
    # loop
    tmp.mdl <- tmp.refit <- tmp.af <- NULL
  }

  # no NULL were returned
  expect_equal(length(mdl.fit), length(model.types))

  ## validate exact same model is returned by directly calling
  ## model functions
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
