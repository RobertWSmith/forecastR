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
#' library(reshape2)
#'
#' data("AirPassengers", package="datasets")
#'
#' split.fctr <- 24
#' ap <- ts.split(AirPassengers, split = split.fctr, as.list=TRUE)
#' ap.ts <- cbind(in.sample = ap$in.sample, out.of.sample = ap$out.of.sample)
#' ap.df <- as.data.frame(ap.ts)
#' ap.df$date <- as.Date(ISOdate(as.integer(time(ap$data)),
#'                               as.integer(cycle(ap$data)), 1))
#' ap.melt.df <- melt(ap.df, id.vars = "date", na.rm = TRUE)
#'
#' ##(ggplot(ap.melt.df, aes(x = date, y = value, color = variable)) + geom_line() +
#' ##   ggtitle("Test/Training Set View"))
#'
#' tsm <- ts.multimodel.fit(AirPassengers, split=split.fctr)
#' mx <- ts.mixture(AirPassengers, tsm, split=split.fctr, oos.h = 12)
#'
#' vals <- cbind(AirPassengers, mx$mixture.model$response, mx$forecast)
#' vals <- window(vals, start=1959)
#' vals.df <- as.data.frame(vals)
#' colnames(vals.df) <- c("Data", "Mixture Model", colnames(mx$forecast))
#' vals.df$date <- as.Date(ISOdate(as.integer(time(vals)),
#'                                 as.integer(cycle(vals)), 1))
#' vals.melt <- melt(vals.df, id.var = "date", na.rm = TRUE)
#' ## (ggplot(vals.melt, aes(x = date, y = value, color = variable)) + geom_line() +
#' ##   ggtitle("All Forecasts"))
#'
#' fcst <- cbind(mx$mixture.model$response, mx$test.forecast)
#' fcst.err <- fcst - ap$out.of.sample
#' colnames(fcst.err) <- c("Mixture Model", colnames(mx$test.forecast))
#' fcst.err.df <- as.data.frame(fcst.err)
#' fcst.err.df$date <- as.Date(ISOdate(as.integer(time(fcst.err)),
#'                                     as.integer(cycle(fcst.err)), 1))
#' fcst.err.melt.df <- melt(fcst.err.df, id.var = "date", na.rm = TRUE)
#' ## (ggplot(fcst.err.melt.df, aes(x = date, y = value, color = variable)) +
#' ##   geom_line() + ggtitle("Error"))
#'
#' fcst.err.melt.df$value <- abs(fcst.err.melt.df$value)
#' ## (ggplot(fcst.err.melt.df, aes(x = date, y = value, color = variable)) +
#' ##   geom_line() + ggtitle("Absolute Error"))
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
