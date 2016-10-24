context("Testing ts.model.fit `multimodel_fit.R`.")

test_that("`ts.multimodel.fit` & `ts.multimodel.refit` & `ts.multimodel.resample` functions", {
  data("AirPassengers", package = "datasets")
  split <- 24
  ap.split <- ts.split(AirPassengers, split = split)
  in.sample <- ap.split$in.sample

  mf <- ts.multimodel.fit(in.sample)
  mf.upd <- ts.multimodel.refit(ap.split$data, mf)
  mr <- ts.multimodel.resample(AirPassengers, mf, boot.reps = 25)

  expect_true(!is.null(mf))
  expect_true(!is.null(mf.upd))
  expect_true(!is.null(mr))
})
