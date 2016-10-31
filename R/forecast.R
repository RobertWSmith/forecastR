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
#' @importFrom stats hasTsp
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
#'
#' ##autoplot(fcst)
forecast <- function(object, h = 18L, ...)
{
  mdl <- model(object)
  fcst <- forecast::forecast(mdl, h=h, ...)

  #convert bounds to time series matrix w/ tsp of fcst$mean
  if ("lower" %in% names(fcst))
  {
    fcst$lower <- hasTsp(as.ts(fcst$lower))
    tsp(fcst$lower) <- tsp(fcst$mean)
  }

  if ("upper" %in% names(fcst))
  {
    fcst$upper <- hasTsp(as.ts(fcst$upper))
    tsp(fcst$upper) <- tsp(fcst$mean)
  }

  return(fcst)
}

#' Baseline / Null Hypothesis Forecasts
#'
#' @param y \code{ts} univariate time series
#' @param h integer. number of periods to forecast
#' @param lambda numeric. parameter for Box-Cox transformation
#' @param ... additional arguments
#' @export
baseline.forecast <- function(y, h = 18L, lambda = optimize.lambda(y), ...)
{
  y.orig <- y
  y.ts <- short.ts(y)
  y <- y.ts$y

  naive.fcst <- forecast::naive(y, h = h, lambda = lambda, ...)
  mean.fcst <- forecast::meanf(y, h = h, lambda = lambda, ...)
  theta.fcst <- forecast::thetaf(y, h = h, ...)

  output <- ts(cbind(
    naive.fcst$mean,
    mean.fcst$mean,
    theta.fcst$mean
    ), start=tsp(y.orig)[2]+deltat(y.orig), frequency=frequency(y.orig))

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
#' @importFrom stats end
#'
#' @export
#'
#' @return \code{mts} of forecast means w/ named columns
multiforecast <- function(object.list, h = 18L, y = NULL, ...)
{
  if (!is.null(y))
  {
    object.list <- ts.multimodel.refit(y, object.list, ...)
    base.fcst <- baseline.forecast(y, h)
  }

  means.fcst <- do.call(cbind, lapply(object.list, function(x) {
    fcst <- forecast(x, h=h)
    ff <- fitted(fcst)
    e <- tsp(ff)[2] + deltat(ff)
    fcst <- ts(as.numeric(fcst$mean), start = e, frequency=frequency(ff))
    return(fcst)
  }))

  colnames(means.fcst) <- sapply(object.list, function(x) {
    if (is.null(x$fit$function.name))
      return(x$function.name)
    return(x$fit$function.name)
  })

  if (!is.null(y))
  {
    mf.names <- colnames(means.fcst)
    means.fcst <- cbind(base.fcst, means.fcst)
    colnames(means.fcst) <- c(colnames(base.fcst), mf.names)
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

