% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is.ts.split}
\alias{is.ts.split}
\title{Tests if object is \code{ts.split}}
\usage{
is.ts.split(x)
}
\arguments{
\item{x}{object to be tested}
}
\value{
\code{TRUE} if \code{x} inherits from \code{ts.split} else
  \code{FALSE}.
}
\description{
Tests if object is \code{ts.split}
}
\examples{
library(forecastR)
x <- ts(1:100, freq=12)
sample.frac <- 0.2
!is.ts.split(x)
x.split <- ts.split(x, split = sample.frac)
is.ts.split(x.split)
}

