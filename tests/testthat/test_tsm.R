library(testthat)
context("Testing `tsm.R`.")

test_that("`tsm` object", {
  ap.time <- as.numeric(time(AirPassengers))
  ap.cycle <- factor(as.integer(cycle(AirPassengers)), labels = c("Jan",
    "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
    "Oct", "Nov", "Dec"))

  ap.lm <- lm(AirPassengers ~ ap.time + ap.cycle)
  ap.tsm <- tsm("lm", ap.lm)

  expect_equivalent(coef(summary(ap.tsm)), coef(summary(ap.lm)))
  expect_equivalent(resid(ap.tsm), resid(ap.lm))
  expect_equivalent(coef(ap.tsm), coef(ap.lm))
})
