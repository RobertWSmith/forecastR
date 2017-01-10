library(forecastR)
library(ggplot2)

data("AirPassengers")

fits <- generate.forecast(AirPassengers)

test.fcst <- cbind(
  AirPassengers,
  fits$forecast,
  fits$forecast[, fits$selected.forecast.name]
)
colnames(test.fcst) <- as.character(
  c(
    "AirPassengers",
    colnames(fits$forecast),
    "selected.forecast"
  )
)

autoplot(test.fcst)
