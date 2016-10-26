## forecast.R


#' Forecast \code{tsm} object model
#'
#' Wrapper for forecast package functions \code{\link[forecast]{forecast}} to
#' allow unified interface.
#'
#' @param object \code{\link[forecastR]{tsm}} fitted model object.
#' @param h integer. number of out of sample forecast periods
#' @param ... additional argument for model function
#'
#' @return \code{\link[forecast]{forecast}} object
#'
#' @seealso \code{\link[forecast]{forecast}}
#'
#' @importFrom forecast forecast
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#'
#' data('AirPassengers', package='datasets')
#' fit <- arima(AirPassengers)
#' fcst <- forecast(fit)
#' autoplot(fcst)
forecast <- function(object, h=18L, ...)
{
  mdl <- model(object)
  log.lambda <- ("lambda" %in% names(mdl)) && is.numeric(mdl$lambda) && as.integer(round(mdl$lambda)) == 0L
  fcst <- forecast::forecast(mdl, h=h, ...)
  if (log.lambda)
  {
    fcst$mean <- fcst$mean - 1.0
    fcst$x <- fcst$x - 1.0
    fcst$lower <- fcst$lower - 1.0
    fcst$upper <- fcst$upper - 1.0
  }
  return(fcst)
}

#' Baseline / Null Hypothesis Forecasts
#'
#' @param y \code{ts} univariate time series
#' @param h integer. number of periods to forecast
#' @param ... additional arguments
#' @export
baseline.forecast <- function(y, h=18L, ...)
{
  y.orig <- y
  y.ts <- short.ts(y)
  y <- y.ts$y

  naive.fcst <- forecast::naive(y, h=h, ...)
  mean.fcst <- forecast::meanf(y, h=h, ...)
  theta.fcst <- forecast::thetaf(y, h=h, ...)

  output <- cbind(naive.fcst$mean, mean.fcst$mean, theta.fcst$mean)
  if (y.ts$was.transformed)
    output <- ts(output, start = tsp(y.orig)[2]+deltat(y.orig), frequency = frequency(y.orig))
  colnames(output) <- c("naive", "mean", "theta")
  return(output)
}

#' Forecast for \code{tsm.multi} objects
#'
#' @param object.list list of \code{tsm} objects
#' @param h integer. length of forecast output
#' @param y \code{ts}. Optional univariate time series, if provided baseline
#'   forecasts are also provided.
#' @param ... additional arguments
#'
#' @export
#'
#' @return \code{mts} of forecast means w/ named columns
multiforecast <- function(object.list, h=18L, y=NULL, ...)
{
  if (!is.null(y))
  {
    object.list <- ts.multimodel.refit(y, object.list, ...)
    base.fcst <- baseline.forecast(y, h)
  }

  means.fcst <- do.call(cbind, lapply(object.list, function(x) {
    fcst <- forecast(x, h=h)
    ff <- fitted(fcst)
    e <- end(ff)
    if (length(e) > 1)
      e <- e[1] + (deltat(ff) * e[2])
    else
      e <- e[1] + deltat(ff)
    fcst <- ts(as.numeric(fcst$mean), start = e, frequency=frequency(ff))
    return(fcst)
  }))
  colnames(means.fcst) <- names(object.list)

  if (!is.null(y))
  {
    means.fcst <- cbind(base.fcst, means.fcst)
    colnames(means.fcst) <- c(colnames(base.fcst), names(object.list))
  }
  return(means.fcst)
}

#' Bootstrap Forecast for model
#'
#' @param model \code{tsm} object fit against sample of data
#' @param y \code{ts} Univariate time series to be bootstrapped
#' @param bootreps integer. number of bootstrap replicates
#' @param split numeric. value passed to \code{\link[forecastR]{ts.split}}
#' @param ... additional arguments
#'
#' @seealso \code{\link[meboot]{meboot}} \code{\link[forecastR]{ts.split}}
#'
#' @importFrom meboot meboot
#' @importFrom forecast tsclean
#'
#' @return \code{bootforecast} object
#'
#' @export
bootforecast <- function(model, y, bootreps = 100, split = 0.20, ...)
  UseMethod("bootforecast")


#' \code{tsm} Bootstrap forecasts
#'
#' @describeIn bootforecast \code{tsm}
bootforecast.tsm <- function(model, y, bootreps = 100, split = 0.20, ...)
{
  y.orig <- y
  y.bs <- meboot::meboot(y, reps = bootreps)$ensemble
  y.bs[y.bs < 0.0] <- 0.0
  y.bs <- y.bs + 1.0
  lambda <- model(model)$lambda
  y.bs <- as.ts(apply(y.bs, 2, forecast::tsclean, lambda = lambda))
  tsp(y.bs) <- tsp(y.orig)
  y.bs.split <- ts.split(y.bs, split = split)

  refits <- apply(y.bs.split$in.sample, 2, function(x) {
    return(ts.model.refit(x, model))
    })
  forecasts <- lapply(refits, function(x) {
    f <- forecast(x, h=nrow(y.bs.split$out.of.sample))
    for (nm in names(f))
    {
      if (nm %in% package_options('ts.fields')[['forecast']])
      {
        if (nm %in% c("mean", "lower", "upper"))
        {
          f[[nm]] <- ts(f[[nm]], start=start(y.bs.split$out.of.sample),
                        frequency = frequency(y.bs.split$out.of.sample))
        }
        else
        {
          f[[nm]] <- ts(f[[nm]], start=start(y.bs.split$in.sample),
                        frequency = frequency(y.bs.split$in.sample))
        }
      }
    }
    return(f)
  })
  forecast.means <- do.call(cbind, lapply(forecasts, function(x) {x$mean}))
}

