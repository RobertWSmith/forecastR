## forecast.R


#' Forecast \code{tsm} object model
#'
#' Wrapper for forecast package functions \code{\link[forecast]{forecast}} to
#' allow unified interface.
#'
#' @param object \code{\link[forecastR]{tsm}} fitted model object.
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
forecast <- function(object, ...)
{
  return(forecast::forecast(model(object), ...))
}

multiforecast <- function(object.list, h=18L, ...)
{
  means.fcst <- do.call(cbind, lapply(object.list, function(x) {
    return(forecast(x, h=h)$mean)
  }))
  colnames(means.fcst) <- names(object.list)
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
  y.boots <- meboot::meboot(y, reps = bootreps)$ensemble
  y.boot.split <- ts.split(y.boots, split = split)
  # y.boot.means <- lapply(y.boot.split, function(x) {
  #   y <- as.ts(rowMeans(x, na.rm=TRUE))
  #   tsp(y) <- tsp(x)
  #   return(y)
  # })
  refits <- apply(y.boot.split$in.sample, 2, function(x) {
    return(ts.model.refit(x, model))
    })
  forecasts <- lapply(refits, function(x) {
    f <- forecast(x, h=nrow(y.boot.split$out.of.sample))
    for (nm in names(f))
    {
      if (nm %in% package_options('ts.fields')[['forecast']])
      {
        if (nm %in% c("mean", "lower", "upper"))
        {
          f[[nm]] <- ts(f[[nm]], start=start(y.boot.split$out.of.sample),
                        frequency = frequency(y.boot.split$out.of.sample))
        }
        else
        {
          f[[nm]] <- ts(f[[nm]], start=start(y.boot.split$in.sample),
                        frequency = frequency(y.boot.split$in.sample))
        }
      }
    }
    return(f)
  })
  forecast.means <- do.call(cbind, lapply(forecasts, function(x) {x$mean}))
}

