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

  ap.short <- window(AirPassengers, end=start(AirPassengers)[1]+1.5)
  ap <- ts.split(ap.short)

  mf <- ts.multimodel.fit(ap$in.sample)
  mf.upd <- ts.multimodel.refit(ap$data, mf)
  mr <- ts.multimodel.resample(ap$data, mf, boot.reps = 25)

  expect_true(!is.null(mf))
  expect_true(!is.null(mf.upd))
  expect_true(!is.null(mr))

  ap.super.short <- window(AirPassengers, end = 1949.3)
  mf <- ts.multimodel.fit(ap$in.sample)
  mf.upd <- ts.multimodel.refit(ap$data, mf)
  mr <- ts.multimodel.resample(ap$data, mf, boot.reps = 25)

  expect_true(!is.null(mf))
  expect_true(!is.null(mf.upd))
  expect_true(!is.null(mr))
})
