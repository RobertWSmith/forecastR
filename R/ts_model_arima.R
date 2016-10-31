## ts_model_arima.R

#' ARIMA model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{Arima}} and
#' \code{\link[forecast]{auto.arima}} to allow unified interface.
#'
#' @param y Univariate Time Series
#' @param lambda optional, if null ignored otherwise treated as the Box-Cox
#'   parameter
#' @param model \code{tsm}, \code{\link[forecast]{Arima}} or
#'   \code{\link[stats]{arima}} object to be refit
#' @param order integer vector. Non-seasonal ARIMA order
#' @param ... additional argument for model function. See
#'   \code{\link[forecast]{Arima}} and \code{\link[forecast]{auto.arima}}
#'   for allowable named arguments and definitions
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{Arima}} and \code{\link[forecast]{auto.arima}}
#'
#' @importFrom forecast Arima auto.arima
#'
#' @family Time Series Models
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- arima(AirPassengers)
#' fit2 <- arima(AirPassengers, D = 1)
#' fit3 <- arima(AirPassengers, lambda = 0)
arima <- function(y, lambda = NULL, model = NULL, order = NULL, ...)
{
  model.name <- "arima"
  kw <- list(...)
  if(!is.null(lambda) && is.na(lambda))
    lambda <- NULL

  if (!is.null(model))
    lambda <- model(model)$lambda

  y.ts <- short.ts(y, freq.multiple = 2, lambda = lambda)
  y <- y.ts$y

  if (!is.null(model))
  {
    lambda <- model(model)$lambda
    fit <- forecast::Arima(y, model = model(model), lambda = lambda, ...)
  } else if (!is.null(order))
  {
    fit <- forecast::Arima(y, order = order, lambda = lambda, ...)
  } else
  {
    fit <- .arima_initial_fit(y, split = 0.20)
  }

  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]), nm)
  }

  output <- tsm(model.name, fit)
  return(output)
}

#' Internal function to determine fit based on small out of sample validation
#'
#' Performs a cross validation against a train / test subeet of the original
#' time series, selecting the model which minimizes mean absolute error.
#'
#' @param y univariate time series
#' @param split numeric. If between < 1, a fraction of samples. If >= 1 a fixed
#'  number of observations
#'
#' @importFrom forecast auto.arima Arima accuracy forecast arimaorder
.arima_initial_fit <- function(y, split = 0.20)
{
  y.orig <- y
  y <- ts.split(y, split = split)
  oos.len <- length(y$out.of.sample)
  opt.lmb <- optimize.lambda(y$in.sample)

  fits <- list(
    base = forecast::auto.arima(y$in.sample),
    lambda = forecast::auto.arima(y$in.sample, lambda = opt.lmb)
  )

  if (length(arimaorder(fits$base)) == 3 || arimaorder(fits$base)[5] == 0)
    fits$seasonal.base <- forecast::auto.arima(y$in.sample, D = 1)

  if (length(arimaorder(fits$base)) == 3 || arimaorder(fits$lambda)[5] == 0)
    fits$seasonal.lambda <- forecast::auto.arima(y$in.sample, D = 1, lambda = opt.lmb)

  fcst.accuracy <- sapply(fits, function(x) {
    fcst <- forecast::forecast(x, h = oos.len)
    acc <- forecast::accuracy(fcst, y$out.of.sample)
    return(acc['Test set', 'MAE'])
  })

  #find minimum error metric & subset models
  best.idx <- which(fcst.accuracy == (min(fcst.accuracy)))[1]
  best.fit <- fits[[best.idx]]
  y <- y.orig
  return(forecast::Arima(y, model = best.fit))
}

