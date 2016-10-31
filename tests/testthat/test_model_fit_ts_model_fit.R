library(testthat)
context("Testing ts.model.fit `model_fit.R`.")

test_that("`ts.model.fit` & `ts.model.refit` functions", {
  ap <- ts.split(AirPassengers, 24)
  model.types <- c("arima", "arfima", "bats", "ets", "nnetar",
    "stlm", "tbats", "tslm")
  no.short.models <- c("bats", "stlm", "tbats")

  mdl.af <- mdl.refit <- mdl.fit <- list()

  # check all models fit data against default arguments
  for (func in model.types)
  {
    # cleanup to ensure not retesting against old values inside
    # loop
    tmp.mdl <- tmp.refit <- tmp.af <- NULL

    tmp.mdl <- ts.model.fit(ap$in.sample, ts.model.type = func)
    tmp.refit <- ts.model.refit(ap$data, model = tmp.mdl)
    tmp.af <- ts.model.autofit(ap$in.sample, ts.model.type = func, return.all.models = FALSE)

    expect_equal(length(tmp.af), 3L, info=func)

    mdl.fit[[func]] <- tmp.mdl
    mdl.refit[[func]] <- tmp.refit
    mdl.af[[func]] <- tmp.af

    ## testing that refit model has equivalent coefficients, with the exception
    ## of ARFIMA models, which do not currently support being refit
    if (!(func %in% c("arfima", "stlm", "tslm")))
    {
      expect_equal(length(coef(tmp.mdl)), length(coef(tmp.refit)), info=func)
    }
  }

  # no NULL were returned
  expect_equal(length(mdl.fit), length(model.types))
  expect_equal(length(mdl.refit), length(model.types))
  expect_equal(length(mdl.af), length(model.types))

  ap.short <- window(AirPassengers, end=start(AirPassengers)[1]+1.5)
  ap <- ts.split(ap.short)

  mdl.af <- mdl.refit <- mdl.fit <- list()
  ## "stlm" will always throw an error where ts length < 2x frequency
  ## "bats" and "tbats" aren't useful with really short time series
  # check all models fit data against default arguments
  for (func in model.types)
  {
    # cleanup to ensure not retesting against old values inside
    # loop
    tmp.mdl <- tmp.refit <- tmp.af <- NULL

    if (func == "stlm")
    {
      tmp.mdl <- expect_error(ts.model.fit(ap$in.sample, ts.model.type = func), info=func)
      tmp.refit <- expect_error(ts.model.refit(ap$data, model = tmp.mdl), info=func)
      tmp.af <- expect_message(ts.model.autofit(ap$in.sample, ts.model.type = func,
                                              return.all.models = FALSE), info=func)
      expect_equal(length(tmp.af), 0L, label = func)
    } else
    {
      tmp.mdl <- ts.model.fit(ap$in.sample, ts.model.type = func)
      tmp.refit <- ts.model.refit(ap$data, model = tmp.mdl)
      tmp.af <- ts.model.autofit(ap$in.sample, ts.model.type = func, return.all.models = FALSE)

      expect_gt(length(tmp.af), 0L, label = func)
    }

    mdl.fit[[func]] <- tmp.mdl
    mdl.refit[[func]] <- tmp.refit
    mdl.af[[func]] <- tmp.af

    ## testing that refit model has equivalent coefficients, with the exception
    ## of ARFIMA models, which do not currently support being refit
    if (!(func %in% c("arfima", "tslm")))
    {
      expect_equal(length(coef(tmp.mdl)), length(coef(tmp.refit)), info=func)
    }
  }

  # STLM does not return for short time series, so we expect one fewer record
  expect_equal(length(mdl.fit), length(model.types) - 1L)
  expect_equal(length(mdl.refit), length(model.types) - 1L)
  expect_equal(length(mdl.af), length(model.types) - 1L)

})
