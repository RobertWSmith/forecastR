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
#' @param return.models logical. if \code{TRUE} then updated models are
#'   returned as `models` field, else updated models are not returned.
#' @param ... extra arguments for `ts.model.type`
#'
#' @importFrom opera mixture
#' @importFrom stats na.omit predict
#' @importFrom ForecastCombinations Forecast_comb Forecast_comb_all
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data("AirPassengers", package="datasets")
#'
#' ap <- window(AirPassengers, end=c(1959,12))
#' ap.oos <- window(AirPassengers, start=c(1960,1))
#' suppressWarnings(autoplot(cbind(ap, ap.oos)))
#'
#' split.fctr <- 24
#' tsm <- ts.multimodel.fit(ap, split=split.fctr)
#'
#' mx <- ts.mixture(ap, tsm, split=split.fctr, oos.h = 12)
#'
#' vals <- cbind(AirPassengers, mx$mixture.model$response, mx$experts)
#' colnames(vals) <- c("Data", "Mixture Model", colnames(mx$experts))
#' suppressWarnings(autoplot(window(vals[,1:2], start=1959)))
#'
#' fcst <- cbind(mx$mixture.model$response, mx$experts)
#' fcst.err <- fcst - ap.oos
#' colnames(fcst.err) <- c("Mixture Model", colnames(mx$experts))
#' suppressWarnings(autoplot(abs(fcst.err)))
#'
#' cum.fcst.err <- ts(do.call(cbind, lapply(abs(fcst.err), cumsum)))
#' tsp(cum.fcst.err) <- tsp(fcst.err)
#' suppressWarnings(autoplot(cum.fcst.err))
#'
#' cum.fcst.err.smry <- as.numeric(cum.fcst.err[nrow(cum.fcst.err), ])
#' print(sort(cum.fcst.err.smry))
ts.mixture <- function(y, tsm.multi = NULL, split = 0.20, oos.h = 18L,
                       alpha = 0.05, boot.reps = NULL,
                       min.records = as.integer(min(6L, frequency(y))+1L),
                       return.models = is.null(tsm.multi), ...)
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
  {
    tsm.multi <- ts.multimodel.fit(y, split = split, ...)
  } else if (return.models && !null.init.tsm.multi)
  {
    tsm.multi <- ts.multimodel.refit(y, tsm.multi = tsm.multi)
  }

  if (!vec.is.short)
  {
    mmr <- ts.multimodel.resample(y, tsm.multi, boot.reps = boot.reps,
                                  split = split, oos.h = oos.h, alpha = alpha)

    mx <- opera::mixture(model = 'MLpol', loss.type = 'square')
    if (mmr$bootstrapped)
    {
      for (i in 1:length(mmr$train.data))
      {
        mx <- predict(mx, newexperts = mmr$test.forecast[[i]],
                      newY = mmr$test.data[[i]], online = TRUE, type = "model")
      }
    } else
    {
      mx <- predict(mx, newexperts = mmr$test.forecast, newY = mmr$test.data,
                    online = TRUE, type = "model")
    }
    mx <- predict(mx, newexperts = mmr$forecast, online = FALSE, type = "all")
    mx$response <- ts(mx$response, start = start(mmr$forecast),
                      frequency = frequency(mmr$forecast))
  } else
  {
    y.split <- ts.split(y, split = split)
    is.fits <- ts.multimodel.refit(y.split$in.sample, tsm.multi, ...)

    mmr <- list(
      y = y,
      forecast = multiforecast(is.fits, h = oos.h, y = y.split$in.sample),
      test.forecast = multiforecast(tsm.multi, h = oos.h, y = y)
      )
    # create a dummy mixture model of simple rowMeans
    mx <- list()
    npreds <- ncol(mmr$experts)
    mx$weights <- matrix(rep(1/npreds, npreds), ncol=npreds)
    mx$response <- ts(rowMeans(mmr$experts), start = start(mmr$forecast),
                      frequency = frequency(mmr$forecast))
  }

  output <- list(
    mixture.model = mx,
    experts = mmr$experts,
    forecast = mmr$forecast,
    test.forecast = mmr$test.forecast
    )
  if ((null.init.tsm.multi) || return.models)
  {
    output$tsm.multi <- tsm.multi
  }
  if (!vec.is.short)
  {
    output$in.sample.experts <- mmr$in.sample.experts
  }

  output <- structure(output, class = "ts.mixture")
  return(output)
}
