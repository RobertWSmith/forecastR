% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformations.R
\name{optimize.lambda}
\alias{optimize.lambda}
\title{Calculate optimal lambda based on \code{tslm} residuals}
\usage{
optimize.lambda(y, lower = -1, upper = 2, tol = 0.001, ...)
}
\arguments{
\item{y}{univariate time series}

\item{lower}{numeric. lower bound parameter for optimization search}

\item{upper}{numeric. upper bound parameter for optimization search}

\item{tol}{numeric. numerical tolerance for optimization search}

\item{...}{additional arguments}
}
\value{
\code{numeric} list, to be passed as `lambda` parameters

numeric. Value to be passed as `lambda` parameter to package functions
  which accept `lambda` keyword arguments.
}
\description{
Identifies lambda for \code{BoxCox} function which mimimzes lag 1 squared
autocorrelation of \code{tslm} residuals.
}
\examples{
library(ggplot2)
library(forecastR)
data("AirPassengers", package = "datasets")

y <- ts.split(AirPassengers)

(lmb <- optimize.lambda(y$in.sample))
a.std <- arima(y$in.sample)
a.lambda <- arima(y$in.sample, lambda = lmb)

f.std <- forecast(a.std, h = length(y$out.of.sample))
f.lambda <- forecast(a.lambda, h = length(y$out.of.sample))

vals <- cbind(actuals = y$out.of.sample, std = f.std$mean, lambda = f.lambda$mean)

## suppressWarnings(autoplot(vals))

fcst.err <- cbind(std = f.std$mean, lambda = f.lambda$mean) - y$out.of.sample
## autoplot(fcst.err)
}

