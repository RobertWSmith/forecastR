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
#' @param lambda nummeric. Box-Cox transformation parameter.
#' @param alpha numeric. significance threshold for \code{\link[forecast]{dm.test}}
#'   which determines if transformation makes for an improved model
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
ts.multimodel.fit <- function(y, split = 0.20, boot.reps = NULL, alpha = 0.05,
                              ts.model.types = c("arfima", "arima", "bats",
                                                 "ets", "nnetar", "stlm",
                                                 "tbats", "tslm"),
                              lambda = NULL, freq.multiple = 2, ...)
{
  y.orig <- y
  kw <- list(...)
  ts.model.types <- sort(unique(match.arg(ts.model.types, several.ok = TRUE)))

  if (!is.null(boot.reps))
  {
    y.bs <- meboot::meboot(y, reps = boot.reps)$ensemble
    y <- rowMeans(y.bs, na.rm = TRUE)
    y[y < 0.0] <- 0.0
    y <- ts(y, start = start(y.orig), frequency = frequency(y.orig))
  }


  if (short.ts.test(ts.split(y)$in.sample))
    ts.model.types <- ts.model.types[!(ts.model.types %in% c("stlm"))]

  if (length(y) < 2L)
    ts.model.types <- c()

  output <- lapply(ts.model.types, function(func.name) {
    return(ts.model.autofit(y, lambda = lambda, split = split, alpha = alpha,
                            ts.model.type = func.name,
                            return.all.models = FALSE, ...))
    })
  names(output) <- ts.model.types
  output <- ts.multimodel.refit(y.orig, structure(.mm.prune(output), class = 'tsm.multi'))
  return(output)
}


# internal funciton to ensure list is not sparse
.mm.prune <- function(mm.fits)
{
  rm.idx <- !(sapply(mm.fits, function(x) {return( is.null(x) || is.null(x$fit))}))
  return(mm.fits[rm.idx])
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
  refit <- lapply(tsm.multi, function(x) {
    return(ts.model.refit(y, model = x, ...))
    })
  names(refit) <- sapply(refit, function(x) { x$function.name })
  return(refit)
}


#' Update & Resample Multimodel Fit
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
ts.multimodel.resample <- function(y, tsm.multi, boot.reps = NULL, split = 0.2,
                                   oos.h = 18L, alpha = 0.05, ...)
{
  dta <- is.fcst <- list()

  y.orig <- y
  y.split <- ts.split(y, split = split)
  in.sample <- y.split$in.sample

  oos.mdl <- ts.multimodel.refit(y, tsm.multi, ...)
  oos.fcst <- multiforecast(oos.mdl, h = oos.h, y = y)

  output <- new.env(parent = emptyenv())
  assign("y", y, envir = output)
  assign("train.data", y.split$in.sample, envir = output)
  assign("test.data", y.split$out.of.sample, envir = output)
  assign("bootstrapped", (!is.null(boot.reps)), envir = output)
  assign("tsm", oos.mdl, envir = output)
  assign("forecast", oos.fcst, envir = output)

  if (!is.null(boot.reps))
  {
    # bootstrap resampling
    y.bs.base <- meboot::meboot(y, reps = ifelse(boot.reps^2 > 999, 999, boot.reps^2))$ensemble
    y.bs.samples <- sapply(1:(boot.reps * 2), function(x) {
      sample(1:ncol(y.bs.base), boot.reps, replace = TRUE)
    })
    y.bs <- apply(y.bs.samples, 2, function(x) {
      tmp <- y.bs.base[,x]
      tmp[tmp < 0.0] <- 0.0
      tmp <- rowMeans(tmp, na.rm = TRUE)
      if (any(tmp < 1))
        tmp <- tmp + 1
      return(tmp)
    })
    y.bs <- as.ts(y.bs)
    tsp(y.bs) <- tsp(y.orig)

    y.bs.split <- ts.split(y.bs, split = split)
    in.sample <- y.bs.split$in.sample

    output$bootstrap.data <- y.bs.split

    dta <- is.fcst <- list()

    for (i in 1:ncol(in.sample))
    {
      is.temp <- in.sample[ ,i]
      oos.temp <- y.bs.split$out.of.sample[ ,i]
      data.temp <- y.bs.split$data[ ,i]

      is.mdl <- ts.multimodel.refit(is.temp, tsm.multi, ...)

      dta[[i]] <- oos.temp
      is.fcst[[i]] <- multiforecast(is.mdl, h = nrow(y.bs.split$out.of.sample), y = data.temp)
    }
    output$test.data <- dta
    output$test.forecast <- is.fcst
  } else
  {
    # no bootstrap resampling
    is.mdl <- ts.multimodel.refit(y.split$in.sample, tsm.multi, ...)
    is.fcst <- multiforecast(is.mdl, h = length(y.split$out.of.sample), y = y.split$in.sample)

    output$test.data <- y.split$out.of.sample
    output$test.forecast <- is.fcst
  }

  output <- structure(as.list(output), class = "tsm.multi.resample")
  return(output)
}
