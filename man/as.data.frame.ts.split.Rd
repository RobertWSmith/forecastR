% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{as.data.frame.ts.split}
\alias{as.data.frame.ts.split}
\title{Modifies \code{ts.split} to a unified \code{data.frame} object}
\usage{
\method{as.data.frame}{ts.split}(x, ...)
}
\arguments{
\item{x}{\code{ts.split} object}

\item{...}{arguments passed on to \code{\link[base]{as.data.frame}}}
}
\description{
Modifies \code{ts.split} to a unified \code{data.frame} object
}
\examples{
library(forecastR)
data('AirPassengers', package='datasets')
split.ex <- ts.split(AirPassengers)
split.df <- as.data.frame(split.ex)
head(split.df)
}
\seealso{
\code{\link[base]{as.data.frame}}
}

