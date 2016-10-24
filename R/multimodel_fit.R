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
#' @param ts.model.types character vector. name of function returing \code{tsm}
#'   model object
#' @param freq.multiple numeric. Multiple of the time series' frequency which
#'  determines if the provided time series is short. If short, `ts.model.types`
#'  are selected from a limited list.
#' @param ... extra arguments for `ts.model.type`
#'
#' @importFrom forecast naive meanf thetaf accuracy
#'
#' @include models.R
#' @export
#'
#' @seealso \code{\link[forecast]{naive}} \code{\link[forecast]{meanf}}
#'   \code{\link[forecast]{thetaf}}
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
ts.multimodel.fit <- function(y, split = 0.20,
                              ts.model.types = c("arfima", "arima", "bats",
                                                 "ets", "nnetar", "stlm",
                                                 "tbats", "tslm"),
                              freq.multiple = package_options("short.ts.frequency.multiple"),
                              ...)
{
  y.orig <- y
  if (!is.null(split))
  {
    y.split <- ts.split(y, split = split)
    y <- y.split$in.sample
    y.oos <- y.split$out.of.sample
  }

  if (short.ts <- short.ts.test(y, freq.multiple = freq.multiple))
  {
    ts.model.type <- sort(unique(package_options("autofit.models.short.ts")))
  } else
  {
    ts.model.types <- sort(unique(match.arg(ts.model.types, several.ok = TRUE)))
  }

  output <- list()

  for (i in 1:length(ts.model.types))
  {
    nm <- ts.model.types[i]
    # print(nm)
    fits <- ts.model.autofit(y, ts.model.type = nm, return.all.models = !is.null(split)) #, ...)
    if (!is.null(split) && !(nm %in% c("bats", "tbats")))
    {
      tmp.acc <- tmp.fcsts <- list()
      for (i in 1:length(fits))
      {
        tmp.fcsts[[i]] <- forecast(fits[[i]], h=length(y.oos))
        tmp.acc[[i]] <- forecast::accuracy(tmp.fcsts[[1]], y.oos)
      }

      best.fcst <- 1
      best.acc <- tmp.acc[[best.fcst]]

      if (length(tmp.fcsts) > 1)
      {
        for (i in 2:length(tmp.fcsts))
        {
          test.acc <- forecast::accuracy(tmp.fcsts[[i]], y.oos)
          if (test.acc['Test set', 'MASE'] < best.acc['Test set', 'MASE'])
          {
            best.fcst <- i
          }
        }
      }
      output[[nm]] <- fits[[best.fcst]]
    } else
    {
      output[[nm]] <- fits
    }
  }
  return(structure(output, class = 'tsm.multi'))
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
  ts.model.types <- sort(unique(names(tsm.multi)))
  output <- list()
  func.names <- NULL

  for (mdl in tsm.multi)
  {
    if (is.null(func.names))
    {
      func.names <- mdl$function.name
    } else
    {
      func.names <- c(func.names, mdl$function.name)
    }
    output[[length(output)+1]] <- ts.model.refit(y, model = mdl, ...)
  }
  names(output) <- func.names
  return(output)
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
ts.multimodel.resample <- function(y, tsm.multi, boot.reps = 100, split = 0.2,
                                   oos.h = 18L, alpha = 0.05, ...)
{
  y.split <- ts.split(y, split=split)
  y.bs <- meboot::meboot(y, reps = boot.reps)$ensemble
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
  oos.fcst <- multiforecast(oos.mdl, h = oos.h)

  for (i in 1:ncol(in.sample))
  {
    is.mdl <- ts.multimodel.refit(y.bs.split$in.sample[ ,i], tsm.multi, ...)

    dta[[i]] <- y.bs.split$out.of.sample[ ,i]
    is.fcst[[i]] <- multiforecast(is.mdl, h = nrow(y.bs.split$out.of.sample))
  }

  dta <- do.call(c, dta)
  is.fcst <- do.call(rbind, is.fcst)

  output <- list(Y = dta, in.sample.experts = is.fcst, out.sample.experts = oos.fcst)
  return(output)
}
