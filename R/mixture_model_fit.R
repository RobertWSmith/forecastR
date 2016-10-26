## mixture_model_fit.R


#' Mixture Model fit
#'
#' @param y \code{\link[stats]{ts}} object. univariate time series
#' @param tsm.multi \code{tsm.multi} object returned from \code{\link[forecastR]{ts.multimodel.fit}}
#' @param split argument passed to \code{ts.split}
#' @param oos.h integer. Out of sample forecast steps to be provided in the
#'   output.
#' @param alpha significance level for comparative testing
#' @param boot.reps integer. number of bootstrap replicates to fit against
#' @param min.records integer. number of records identifying a minimum sample for
#'   splitting the time series into subsets for in-sample cross validation of the
#'   model fits
#' @param ... extra arguments for `ts.model.type`
#'
#' @importFrom opera mixture
#' @importFrom stats na.omit predict
#'
#' @include models.R
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data("AirPassengers", package="datasets")
#'
#' ap <- window(AirPassengers, end=c(1959,12))
#' ap.oos <- window(AirPassengers, start=c(1960,1))
#' autoplot(cbind(ap, ap.oos))
#'
#' split.fctr <- 24
#' tsm <- ts.multimodel.fit(ap, split=split.fctr)
#'
#' mx <- ts.mixture(ap, tsm, split=split.fctr, oos.h = 12)
#'
#' vals <- cbind(AirPassengers, mx$mixture.model$response, mx$out.of.sample.experts)
#' colnames(vals) <- c("Data", "Mixture Model", colnames(mx$out.of.sample.experts))
#' autoplot(window(vals[,1:2], start=1959))
#'
#' fcst <- cbind(mx$mixture.model$response, mx$out.of.sample.experts)
#' fcst.err <- fcst - ap.oos
#' colnames(fcst.err) <- c("Mixture Model", colnames(mx$out.of.sample.experts))
#' autoplot(abs(fcst.err))
#'
#' cum.fcst.err <- ts(do.call(cbind, lapply(abs(fcst.err), cumsum)))
#' tsp(cum.fcst.err) <- tsp(fcst.err)
#' autoplot(cum.fcst.err)
#' cum.fcst.err.smry <- as.numeric(cum.fcst.err[nrow(cum.fcst.err), ])
#' print(sort(cum.fcst.err.smry))
ts.mixture <- function(y, tsm.multi = NULL, split = 0.2, oos.h = 18L,
                       alpha = 0.05, boot.reps = 25,
                       min.records = as.integer(min(12L, frequency(y))+1L), ...)
{
  vec.is.short <- (length(y) < min.records)
  if (vec.is.short)
  {
    split <- NULL
  } else if (split > length(y))
  {
    # if the split is longer than the length of y, split is assigned to 20% of y
    y <- 0.20
  }

  null.init.tsm.multi <- is.null(tsm.multi)
  if (null.init.tsm.multi)
    tsm.multi <- ts.multimodel.fit(y, split = split, ...)

  if (!vec.is.short)
  {
    mmr <- ts.multimodel.resample(y, tsm.multi, boot.reps = 25, split = split,
                                  oos.h=oos.h, alpha=alpha)
    mx <- opera::mixture(Y = mmr$Y, experts = mmr$in.sample.experts)
    mx <- predict(mx, newexperts = mmr$out.of.sample.experts, online=FALSE, type="all")
    mx$response <- ts(mx$response, start=start(mmr$out.of.sample.experts),
                      frequency = frequency(mmr$out.of.sample.experts))
  } else
  {
    mmr <- list(
      Y = y,
      out.of.sample.experts = multiforecast(tsm.multi, h = oos.h, y = y)
      )
    # create a dummy mixture model of simple rowMeans
    mx <- list()
    npreds <- ncol(mmr$out.of.sample.experts)
    mx$weights <- matrix(rep(1/npreds, npreds), ncol=npreds)
    mx$response <- as.ts(rowMeans(mmr$out.of.sample.experts))
    tsp(mx$response) <- tsp(mmr$out.of.sample.experts)
  }

  output <- list()
  if (null.init.tsm.multi)
    output$tsm.multi <- tsm.multi

  output$mixture.model <- mx

  if (!vec.is.short)
    output$in.sample.experts <- mmr$in.sample.experts

  output$out.of.sample.experts <- mmr$out.of.sample.experts

  output <- structure(output, class = "ts.mixture")
  return(output)
}
