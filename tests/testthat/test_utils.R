context("Testing `utils.R`.")

test_that("`ts.split` function", {
  data("AirPassengers", package = "datasets")
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

  # df.list <- as.data.frame(split.list)
  # df.ts <- as.data.frame.ts.split(split.ts)
  # expect_equal(nrow(df.list), nrow(df.ts))
  # expect_equal(ncol(df.list), ncol(df.ts))
  # tidy.list <- tidy(split.list)
  # tidy.ts <- tidy(split.ts)
  # expect_equal(nrow(df.list), nrow(tidy.list))
  # expect_equal(ncol(df.list), ncol(tidy.list))
  # expect_equal(nrow(df.ts), nrow(tidy.ts))
  # expect_equal(ncol(df.ts), ncol(tidy.ts))

  split <- 24
  sl <- ts.split(AirPassengers, split = split, as.list=TRUE)
  expect_equal(length(sl$out.of.sample), split)
  expect_equal(length(sl$in.sample), (length(AirPassengers)-split))
  expect_equal(length(sl$data), length(AirPassengers))

  sm <- ts.split(AirPassengers, split = split, as.list=FALSE)
  expect_equal(length(sm[,3]), length(AirPassengers))
  expect_equal(length(na.omit(sm[,3])), split)
})
