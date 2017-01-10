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
#' @importFrom forecast accuracy
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
#' (ggplot(ap.melt.df, aes(x = date, y = value, color = variable)) +
#'   geom_line() +
#'   ggtitle("Test/Training Set View"))
#'
#' tsm <- ts.multimodel.fit(AirPassengers, split=split.fctr)
#' mx <- ts.mixture(AirPassengers, tsm, split=split.fctr, oos.h = 12)
#'
#' vals <- cbind(AirPassengers, mx$mixture.forecast, mx$forecast)
#' vals <- window(vals, start=1959)
#' vals.df <- as.data.frame(vals)
#' colnames(vals.df) <- c("Data", "Mixture Model", as.character(colnames(mx$forecast)))
#' vals.df$date <- as.Date(ISOdate(as.integer(time(vals)),
#'                                 as.integer(cycle(vals)), 1))
#' vals.melt <- melt(vals.df, id.var = "date", na.rm = TRUE)
#' (ggplot(vals.melt, aes(x = date, y = value, color = variable)) +
#'   geom_line() +
#'   ggtitle("All Forecasts"))
#'
#' fcst <- cbind(mx$mixture.forecast, mx$test.forecast)
#' fcst.err <- fcst - ap$out.of.sample
#' colnames(fcst.err) <- c("Mixture Model", colnames(mx$test.forecast))
#' fcst.err.df <- as.data.frame(fcst.err)
#' fcst.err.df$date <- as.Date(ISOdate(as.integer(time(fcst.err)),
#'                                     as.integer(cycle(fcst.err)), 1))
#' fcst.err.melt.df <- melt(fcst.err.df, id.var = "date", na.rm = TRUE)
#' (ggplot(fcst.err.melt.df, aes(x = date, y = value, color = variable)) +
#'   geom_line() +
#'   ggtitle("Error"))
#'
#' fcst.err.melt.df$value <- abs(fcst.err.melt.df$value)
#' (ggplot(fcst.err.melt.df, aes(x = date, y = value, color = variable)) +
#'   geom_line() +
#'   ggtitle("Absolute Error"))
#'
ts.mixture <- function(y, tsm.multi = NULL, split = 0.20, oos.h = 18L,
                       alpha = 0.05, boot.reps = NULL, ...)
{
  output <- new.env(parent = emptyenv())
  assign("y", y, envir = output)
  assign("split", split, envir = output)
  assign("alpha", alpha, envir = output)
  assign("oos.h", oos.h, envir = output)
  assign("boot.reps", boot.reps, envir = output)
  assign("experts", tsm.multi, envir = output)

  null.init.tsm.multi <- is.null(tsm.multi)
  output$y.split <- y.split <- ts.split(y, split = split)
  output$is.short <- vec.is.short <- short.ts.test(y.split$in.sample)

  if (null.init.tsm.multi)
    tsm.multi <- ts.multimodel.fit(y, split = split, ...)

  # ensure models are refit to whole dataset
  tsm.multi <- ts.multimodel.refit(y, tsm.multi = tsm.multi)

  ## grab out of sample forecast, checking if any values are not acceptable
  mf <- multiforecast(tsm.multi, h = oos.h)
  if (length(dim(mf)) == 2L)
  {
    good.est <- apply(mf, 2, function(z) {
      return(!any(is.infinite(z) | is.na(z) | is.null(z) | abs(z) > max(y)^2))
    })
    output$tsm.multi <- tsm.multi <- tsm.multi[good.est]
  }

  if (!vec.is.short)
  {
    mmr <- ts.multimodel.resample(y, tsm.multi, boot.reps = boot.reps,
                                  split = split, oos.h = oos.h, alpha = alpha)

    mx <- opera::mixture(model = 'Ridge', loss.type = 'square')
    if (mmr$bootstrapped)
    {
      #train.data & test.data are lists of time series
      for (i in 1:length(mmr$train.data))
      {
        alive.fcst <- !(is.infinite(mmr$test.forecast[[i]]) ||
                          is.na(mmr$test.forecast[[i]]) ||
                          is.null(mmr$test.forecast[[i]]))
        alive.fcst <- ifelse(alive.fcst, 1, 0)
        mx <- predict(mx, newexperts = mmr$test.forecast[[i]], alive = alive.fcst,
                      newY = mmr$test.data[[i]], online = TRUE, type = "model")
      }
    } else
    {
      alive.fcst <- !(is.infinite(mmr$test.forecast) ||
                        is.na(mmr$test.forecast) ||
                        is.null(mmr$test.forecast))
      mx <- predict(mx, newexperts = mmr$test.forecast, newY = mmr$test.data,
                    alive = alive.fcst, online = TRUE, type = "model")
    }
    mxr <- predict(mx, newexperts = mmr$forecast, online = FALSE, type = "all")
    mxr$response <- ts(mxr$response, start = start(mmr$forecast),
                      frequency = frequency(mmr$forecast))
  } else
  {
    y.split <- ts.split(y, split = split)
    is.fits <- ts.multimodel.refit(y.split$in.sample, tsm.multi, ...)

    mmr <- list(
      y = y,
      forecast = multiforecast(tsm.multi, h = oos.h, y = y),
      test.forecast = multiforecast(is.fits, h = length(y.split$out.of.sample),
                                    y = y.split$in.sample)
      )
    # create a dummy mixture model of simple rowMeans
    mx <- list()
    npreds <- ncol(mmr$forecast)
    mx$weights <- matrix(rep(1/npreds, npreds), ncol = npreds)
    mx$response <- ts(rowMeans(mmr$forecast), start = start(mmr$forecast),
                      frequency = frequency(mmr$forecast))
    mxr <- mx
  }

  output$mixture.model <- mx
  output$mx.model <- mxr
  output$experts <- tsm.multi
  output$test.forecast <- mmr$test.forecast
  output$mixture.forecast <- mxr$response

  fcst <- mmr$forecast
  output$forecast <- cbind(fcst, mxr$response)
  colnames(output$forecast) <- c(colnames(fcst), "mixture.model")

  output <- structure(as.list(output), class = "ts.mixture")
  return(output)
}


#' Final forecast selection
#'
#' @param y time series vector
#' @param ts.mixture object of class 'tsm'
#' @param split fraction of records to reserve for our of sample cross validation
#' @param ... other keyword arguments, not currently used
final.forecast.selection <- function(y, ts.mixture, split = 0.10, ...) {
  stopifnot(inherits(ts.mixture, "ts.mixture"))
  mx.nm <- "mixture.model"

  y.split <- ts.split(y, split = split)

  if (length(y) < 3 || !inherits(ts.mixture$mixture.model, "mixture"))
  {
    ts.mixture$selected.forecast <- ts.mixture$mixture.forecast
    ts.mixture$selected.forecast.name <- mx.nm

    tfn <- colnames(ts.mixture$test.forecast)
    ts.mixture$test.forecast <- cbind(ts.mixture$test.forecast, ts.mixture$mixture.forecast)
    colnames(ts.mixture$test.forecast) <- c(tfn, mx.nm)

  } else
  {
    tsm.multi <- ts.multimodel.refit(y.split$in.sample, tsm.multi = ts.mixture$experts)
    mf <- multiforecast(tsm.multi, h = length(y.split$out.of.sample), y = y.split$in.sample)
    resp <- predict(ts.mixture$mixture.model, newexperts = mf, online = FALSE, type = "all")
    resp <- ts(resp$response, start = start(mf), frequency = frequency(mf))

    fcst <- cbind(mf, resp)
    colnames(fcst) <- c(colnames(mf), mx.nm)
    fcst.err <- fcst - y.split$out.of.sample
    colnames(fcst.err) <- colnames(fcst)

    fcst.err.sq <- colSums(fcst.err^2, na.rm = TRUE)
    names(fcst.err.sq) <- colnames(fcst)

    fcst.err.stat <- apply(fcst, 2, function(ft) {
      acc <- forecast::accuracy(ft, y.split$out.of.sample)
      acc <- acc['Test set', 'RMSE']
      return(acc)
    })
    min.fcst.err.stat <- fcst.err.stat[fcst.err.stat == min(fcst.err.stat)]

    min.fcst.err <- as.character( names(min.fcst.err.stat)[1] )

    ts.mixture$test.forecast <- fcst
    ts.mixture$residuals <- tidy_ts_df(fcst.err)

    ts.mixture$residuals$selected.forecast <- fcst.err[,min.fcst.err]
    ts.mixture$residuals$selected.forecast.name <- min.fcst.err

    ts.mixture$selected.forecast <- ts.mixture$forecast[ , min.fcst.err[1] ]
    ts.mixture$selected.forecast.name <- min.fcst.err[1]
  }

  return(ts.mixture)
}


#' Generate forecast
#'
#' @param y time series vector
#' @param boot.reps number of maximum entropy bootstrap replicates. If \code{NULL}, then no bootstrapping is applied.
#' @param oos.h integer. number of out of sample forecast steps
#' @param split fraction of observations from \code{y} which should be reserved for cross validation
#' @param alpha significance threshold
#' @param ... other keyword arguments, not currently used
#'
#' @export
generate.forecast <- function(y, boot.reps = NULL, oos.h = 18L, split = 0.20, alpha = 0.05, ...) {
  y.split <- ts.split(y, split = split)

  tsm <- ts.multimodel.fit(y, boot.reps = boot.reps)
  mx <- ts.mixture(y, tsm, oos.h = oos.h, boot.reps = boot.reps)

  ff.split <- 6L
  ## if the length of the out of sample portion is shorter than the ff.split
  ## default final split
  ## then cut the length of the out of sample in half
  if (length(y.split$out.of.sample) < ff.split)
    ff.split <- as.numeric(length(y.split$out.of.sample)) / 2.0

  ff.split <- as.integer(floor(ff.split))
  ffs <- final.forecast.selection(y, mx, split = ff.split)

  return(ffs)
}

