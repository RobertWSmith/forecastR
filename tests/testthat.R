library(testthat)
library(forecastR)

requireNamespace("datasets")
requireNamespace("forecast")
#requireNamespace("meboot")

data("AirPassengers", package = "datasets")

test_check("forecastR")
