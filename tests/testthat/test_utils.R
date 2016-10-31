library(testthat)
context("Testing `utils.R`.")

test_that("`ts.split` function", {
  frac <- 0.25

  split.list <- ts.split(AirPassengers, split = frac, as.list = TRUE)

  expect_equal(length(split.list$data), length(AirPassengers))
  expect_equal(length(split.list$out.of.sample), length(AirPassengers) * frac)
  expect_equal(length(split.list$in.sample), length(AirPassengers) * (1 - frac))

  expect_is(split.list, "ts.split")
  expect_is(split.list, "list")
  expect_true(is.ts.split(split.list))

  split.ts <- ts.split(AirPassengers, split = frac, as.list = FALSE)

  expect_equal(nrow(split.ts), length(AirPassengers))
  expect_equal(length(split.ts[, "data"]), length(AirPassengers))
  expect_equal(length(split.ts[, "out.of.sample"]), length(AirPassengers))
  expect_equal(length(split.ts[, "in.sample"]), length(AirPassengers))
  expect_equal(length(na.omit(split.ts[, "data"])), length(AirPassengers))
  expect_equal(length(na.omit(split.ts[, "out.of.sample"])), length(AirPassengers) * frac)
  expect_equal(length(na.omit(split.ts[, "in.sample"])), length(AirPassengers) * (1 - frac))

  expect_is(split.ts, "ts.split")
  expect_is(split.ts, "ts")
  expect_is(split.ts, "mts")
  expect_true(is.ts.split(split.ts))

  split <- 24
  sl <- ts.split(AirPassengers, split = split, as.list=TRUE)

  expect_equal(length(sl$out.of.sample), split)
  expect_equal(length(sl$in.sample), (length(AirPassengers)-split))
  expect_equal(length(sl$data), length(AirPassengers))

  sm <- ts.split(AirPassengers, split = split, as.list=FALSE)

  expect_equal(length(sm[,3]), length(AirPassengers))
  expect_equal(length(na.omit(sm[,3])), split)
})
