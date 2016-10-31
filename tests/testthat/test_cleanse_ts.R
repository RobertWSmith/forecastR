library(testthat)
context("Testing `cleanse.R`.")

test_that("`cleanse` & `inv.cleanse` function", {
  y <- AirPassengers
  cl <- cleanse(y)
  ic <- inv.cleanse(cl)

  expect_equal(ic, y, tolerance = 0.01)

  y2 <- y - median(y)
  cl <- cleanse(y2)
  ic <- inv.cleanse(cl)

  expect_true(all(cl > 0.0))
  expect_equal(ic, y2, tolerance = 0.01)
})
