## multimodel_fit.R

#' Multi- Time Series Model Fit
#'
#' Fits all models in `ts.model.types`, with the addition of providing a naive
#' forecast, a sample mean forecast and fitting a theta model forecast. Relative
#' Error returned is defined as `error <- estimate - actual`.
#'
#' @param y \code{\link[stats]{ts}} object. univariate time series
#' @param split numeric. Determines in/out-sample proportions. If NULL, no split
#'   is applied.
#' @param boot.reps integer. Optional parameter, if provided model is fit against
#'   the mean of `boot.reps` number of bootstrap replicates. Can help smooth
#'   excess volatility
#' @param ts.model.types character vector. name of function returing \code{tsm}
#'   model object
#' @param freq.multiple numeric. Multiple of the time series' frequency which
#'  determines if the provided time series is short. If short, `ts.model.types`
#'  are selected from a limited list.
#' @param ... extra arguments for `ts.model.type`
#'
#' @importFrom forecast accuracy tsclean
#' @importFrom meboot meboot
#'
#' @export
#'
#' @seealso \code{\link[forecast]{naive}} \code{\link[forecast]{meanf}}
#'   \code{\link[forecast]{thetaf}} \code{\link[meboot]{meboot}}
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' ap.split <- ts.split(AirPassengers, split = 24)
#' in.sample <- ap.split$in.sample
#' mf <- ts.multimodel.fit(in.sample)
#' for (nm in names(mf))
#' {
#'   print(mf[[nm]])
#' }
ts.multimodel.fit <- function(y, split = 0.20, boot.reps = NULL,
                              ts.model.types = c("arfima", "arima", "bats",
                                                 "ets", "nnetar", "stlm",
                                                 "tbats", "tslm"),
                              freq.multiple = package_options("short.ts.frequency.multiple"),
                              ...)
{
  y.orig <- y
  if (!is.null(boot.reps))
  {
    y <- as.ts(rowMeans(meboot::meboot(y, reps = boot.reps)$ensemble, na.rm=TRUE))
    y[y < 0.0] <- 0.0
    tsp(y) <- tsp(y.orig)
  }

  if (!is.null(split))
  {
    y.split <- ts.split(y, split = split)
    y <- y.split$in.sample
    y.oos <- y.split$out.of.sample
  }

  if (short.ts <- short.ts.test(y, freq.multiple = freq.multiple))
  {
    ts.model.types <- sort(unique(package_options("autofit.models.short.ts")))
  } else
  {
    ts.model.types <- sort(unique(match.arg(ts.model.types, several.ok = TRUE)))
  }

  output <- list()
  for (i in 1:length(ts.model.types))
  {
    nm <- ts.model.types[i]
    fits <- ts.model.autofit(y, ts.model.type = nm, return.all.models = !is.null(split))#, ...)
    if (!is.null(split) && !(nm %in% c("bats", "tbats")))
    {
      tmp.acc <- tmp.fcsts <- list()
      for (i in 1:length(fits))
      {
        tmp.fcsts[[i]] <- forecast(fits[[i]], h=length(y.oos))
        if (!short.ts)
        {
          tmp.acc[[i]] <- forecast::accuracy(tmp.fcsts[[1]], y.oos)
        }
        else
        {
          tmp.acc[[i]] <- forecast::accuracy(tmp.fcsts[[1]])
        }
      }

      best.fcst <- 1
      best.acc <- tmp.acc[[best.fcst]]
      if (length(tmp.fcsts) > 1)
      {
        for (i in 2:length(tmp.fcsts))
        {
          if (!short.ts)
          {
            test.acc <- forecast::accuracy(tmp.fcsts[[i]], y.oos)
            if (test.acc['Test set', 'MASE'] < best.acc['Test set', 'MASE'])
            {
              best.fcst <- i
            }
          } else
          {
            test.acc <- forecast::accuracy(tmp.fcsts[[i]])
            if (test.acc['Training set', 'MAPE'] < best.acc['Training set', 'MAPE'])
            {
              best.fcst <- i
            }
          }
        }
      }
      output[[nm]] <- fits[[best.fcst]]
    } else
    {
      output[[nm]] <- fits
    }
  }
  output <- structure(output, class = 'tsm.multi')
  return(output)
}


#' Update Multimodel fit
#'
#' @param y \code{ts}. Univariate Time Series
#' @param tsm.multi \code{tsm.multi} object. Model which was fit in call to
#'   \code{\link[forecastR]{ts.multimodel.fit}}
#' @param ... additional keyword arguments provided to `ts.model.type` function.
#'
#' @importFrom stats as.ts
#'
#' @return \code{tsm.mulit} object, with subspecialization determined by
#'   `ts.model.type` argument.
#'
#' @seealso \code{\link[forecastR]{ts.multimodel.fit}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#'
#' ap.split <- ts.split(AirPassengers, split = 24)
#' in.sample <- ap.split$in.sample
#'
#' mf <- ts.multimodel.fit(in.sample)
#'
#' mf.upd <- ts.multimodel.refit(ap.split$data, mf)
ts.multimodel.refit <- function(y, tsm.multi, ...)
{
  fcst.names <- character()
  for (i in 1:length(tsm.multi))
  {
    mdl <- tsm.multi[[i]]
    tsm.multi[[i]] <- ts.model.refit(y, model = mdl, ...)
  }
  return(tsm.multi)
}


#' Update Multimodel Fit
#'
#' @param y \code{ts}. Univariate Time Series
#' @param tsm.multi \code{tsm.multi} object. Model which was fit in call to
#'   \code{\link[forecastR]{ts.model.fit}}
#' @param boot.reps integer. Number of bootstrap replicates to test against
#' @param split numeric. Determines in/out-sample proportions
#' @param oos.h integer. Number of out of sample forecast steps
#' @param alpha numeric. Significance threshold for internal testing
#' @param ... additional keyword arguments provided to \code{\link[meboot]{meboot}}
#'   and the models when calling their update methods.
#'
#' @importFrom stats as.ts
#' @importFrom meboot meboot
#' @importFrom forecast tsclean
#'
#' @return \code{tsm} object, with subspecialization determined by `ts.model.type`
#' argument.
#'
#' @seealso \code{\link[forecastR]{ts.model.fit}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#'
#' mmf <- ts.multimodel.fit(AirPassengers)
#'
#' mmr <- ts.multimodel.resample(AirPassengers, mmf, boot.reps = 25)
ts.multimodel.resample <- function(y, tsm.multi, boot.reps = 25, split = 0.2,
                                   oos.h = 18L, alpha = 0.05, ...)
{
  y.orig <- y
  y.split <- ts.split(y, split=split)
  y.bs <- meboot::meboot(y, reps = boot.reps)$ensemble
  y.bs[y.bs < 0.0] <- 0.0
  y.bs <- y.bs + 1
  y.bs <- as.ts(apply(y.bs, 2, forecast::tsclean, lambda = 0))

  tsp(y.bs) <- tsp(y.orig)
  if (!is.null(split))
  {
    y.bs.split <- ts.split(y.bs, split=split)
    in.sample <- y.bs.split$in.sample
  } else
  {
    in.sample <- y.bs
  }

  dta <- is.fcst <- output <- list()

  oos.mdl <- ts.multimodel.refit(y.split$data, tsm.multi, ...)
  oos.fcst <- multiforecast(oos.mdl, h = oos.h, y = y.split$data)

  for (i in 1:ncol(in.sample))
  {
    is.mdl <- ts.multimodel.refit(in.sample[ ,i], tsm.multi, ...)

    dta[[i]] <- y.bs.split$out.of.sample[ ,i]
    is.fcst[[i]] <- multiforecast(is.mdl, h = nrow(y.bs.split$out.of.sample),
                                  y=y.bs.split$data[ ,i])
  }

  dta <- do.call(c, dta)
  is.fcst <- do.call(rbind, is.fcst)

  output <- list(Y = dta, in.sample.experts = is.fcst, out.of.sample.experts = oos.fcst)
  output <- structure(output, class="tsm.multi.resample")
  return(output)
}
