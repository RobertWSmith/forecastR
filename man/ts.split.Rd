% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ts.split}
\alias{ts.split}
\title{Splits time series into train & test subsets}
\usage{
ts.split(x, split = 0.2, as.list = TRUE, short.ts.multiple = 2, ...)
}
\arguments{
\item{x}{Univariate Time Series}

\item{split}{numeic. If between 0 and 1, modeled as a percentage of out of
sample observations. If greater than 1 then number is rounded, and used
as an explicit number of out of sample records. Defaults to 20\% of records
out of sample.}

\item{as.list}{logical. Default \code{TRUE} returns a list with 3 elements,
if \code{FALSE}, returns a \code{mts} object with 3 columns}

\item{short.ts.multiple}{numeric. Multiple of \code{frequency(x)} which
if \code{frequency(x) * short.ts.multiple > length(x)} indicates that a
time series is short. Defaults to 2. Controls attribute \code{is.short}.}

\item{...}{additional options, not currently used}
}
\value{
\code{ts.split} object.
}
\description{
Splits time series into train & test subsets
}
\examples{
library(forecastR)
x <- ts(1:100, freq=12)
frac <- 0.20
x.split <- ts.split(x, split = frac)

x.data <- x.split$data
x.in.sample <- x.split$in.sample
x.out.of.sample <- x.split$out.of.sample

length(na.omit(x.out.of.sample)) == (length(x.data) * frac)
x.data == c(na.omit(x.in.sample), na.omit(x.out.of.sample))

data("AirPassengers", package="datasets")
split <- 24L
ap.split <- ts.split(AirPassengers, split = split)
length(ap.split$data) == length(AirPassengers)
length(ap.split$out.of.sample) == split
length(ap.split$in.sample) == (length(AirPassengers) - split)
}

