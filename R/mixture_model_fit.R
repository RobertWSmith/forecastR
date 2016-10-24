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
#' cum.fcst.err <- ts(do.call(cbind, lapply(abs(fcst.err), cumsum)))
#' tsp(cum.fcst.err) <- tsp(fcst.err)
#' autoplot(cum.fcst.err)
ts.mixture <- function(y, tsm.multi, split = 0.2, oos.h = 18L, alpha = 0.05, boot.reps = 25, ...)
{
  mmr <- ts.multimodel.resample(y, tsm.multi, boot.reps = 25, split=split,
                                oos.h=oos.h, alpha=alpha)
  mx <- opera::mixture(Y = mmr$Y, experts = mmr$in.sample.experts)
  mx <- predict(mx, newexperts = mmr$out.sample.experts, online=FALSE, type="all")
  mx$response <- ts(mx$response, start=start(mmr$out.sample.experts),
                    frequency = frequency(mmr$out.sample.experts))
  return(
    list(
      mixture.model = mx,
      in.sample.experts = mmr$in.sample.experts,
      out.of.sample.experts = mmr$out.sample.experts
      )
    )
}
