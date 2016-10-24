## models.R


.grab.func.name <- function()
{
  return(deparse(sys.calls()[[sys.nframe() - 1]]))
}


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
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y

  if (!is.null(model))
  {
    fit <- forecast::Arima(y, model = model(model), lambda = lambda, ...)
  } else if (!is.null(order))
  {
    fit <- forecast::Arima(y, order = order, lambda = lambda, ...)
  } else
  {
    fit <- forecast::auto.arima(y, lambda = lambda, ...)
  }

  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  }

  output <- tsm(model.name, fit)
  return(output)
}


#' ARFIMA model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{arfima}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param model \code{tsm} object, used to refit model
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{arfima}}
#'
#' @importFrom forecast arfima
#' @importFrom stats is.ts
#'
#' @family Time Series Models
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- arfima(AirPassengers)
#' fit2 <- arfima(AirPassengers, lambda = 0)
arfima <- function(y, model = NULL, ...)
{
  model.name <- "arfima"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y

  if (!is.null(model))
  {
    lambda <- model(model)$lambda
  }

  fit <- forecast::arfima(y, ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  }
  output <- tsm(model.name, fit)
  return(output)
}


#' BATS model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{bats}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param model previously fitted model object.
#' @param lambda not used, in place to ensure uniform API
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{bats}}
#'
#' @importFrom forecast bats
#' @importFrom stats is.ts
#'
#' @export
#'
#' @family Time Series Models
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- bats(AirPassengers)
#' fit2 <- bats(AirPassengers, lambda = 0)
bats <- function(y, model = NULL, lambda = NULL, ...)
{
  model.name <- "bats"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::bats(y, model = model(model), ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  }
  output <- tsm(model.name, fit)
  return(output)
}


#' ETS model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{ets}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param model either a string identifying model method, or previously fitted
#'   model object. See \code{\link[forecast]{ets}} for more information
#' @param allow.multiplicative.trend logical. should the model consider
#'   multiplicative exponential smoothing models?
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{ets}}
#'
#' @importFrom forecast ets
#' @importFrom stats is.ts
#'
#' @family Time Series Models
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- ets(AirPassengers)
#' fit2 <- ets(AirPassengers, lambda = 0)
ets <- function(y, model = "ZZZ", allow.multiplicative.trend = TRUE, ...)
{
  model.name <- "ets"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::ets(y, model = model(model),
                       allow.multiplicative.trend = allow.multiplicative.trend, ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  }
  output <- tsm(model.name, fit)
  return(output)
}


#' NNETAR model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{nnetar}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param model previously fit model object
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{nnetar}}
#'
#' @importFrom forecast nnetar
#' @importFrom stats frequency tsp tsp<- ts is.ts
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' ap.split <- ts.split(AirPassengers)
#' out.sample.len <- length(ap.split$out.of.sample)
#'
#' #base fit
#' fit <- nnetar(ap.split$in.sample)
#' #log transformation
#' fit2 <- nnetar(ap.split$in.sample, lambda = 0)
#' #log transformation w/ nnet decay parameter
#' fit3 <- nnetar(ap.split$in.sample, lambda = 0, decay = 0.01)
#'
#' fcst <- forecast(fit, h = out.sample.len)
#' fcst2 <- forecast(fit2, h = out.sample.len)
#' fcst3 <- forecast(fit3, h = out.sample.len)
#'
#' vals <- window(cbind(
#'  data = ap.split$data,
#'  base = fcst$mean,
#'  lambda = fcst2$mean,
#'  decay = fcst3$mean
#'  ), start = c(1957,1))
#'
#' autoplot(vals, na.rm = TRUE)
nnetar <- function(y, model = NULL, ...)
{
  model.name <- "nnetar"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::nnetar(y, model = model(model), ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  }
  output <- tsm(model.name, fit)
  return(output)
}


#' STLM model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{stlm}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param model previously fit model object, currently not used
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{stlm}}
#'
#' @importFrom forecast tbats
#' @importFrom stats is.ts
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- stlm(AirPassengers)
#' fit2 <- stlm(AirPassengers, lambda = 0)
stlm <- function(y, model = "ZZN", ...)
{
  model.name <- "stlm"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::stlm(y, ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  }
  output <- tsm(model.name, fit)
  return(output)
}


#' TBATS model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{tbats}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param lambda not used, in place to ensure uniform API
#' @param model previously fit model
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{tbats}}
#'
#' @importFrom forecast tbats
#' @importFrom stats is.ts
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- nnetar(AirPassengers)
#' fit2 <- nnetar(AirPassengers, lambda = 0)
tbats <- function(y, lambda = NULL, model = NULL, ...)
{
  model.name <- "tbats"
  y.orig <- y
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::tbats(y, model = model(model), ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  }
  output <- tsm(model.name, fit)
  return(output)
}


#' Time Series Linear Model
#'
#' Wrapper for forecast package functions \code{\link[forecast]{tslm}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param model previously fit model
#' @param ... additional arguments to \code{\link[forecast]{tslm}}
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{tslm}}
#'
#' @importFrom forecast tslm
#' @importFrom stats as.formula
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- nnetar(AirPassengers)
#' fit2 <- nnetar(AirPassengers, lambda = 0)
tslm <- function(y, model = NULL, ...)
{
  model.name <- "tslm"

  y.orig <- y
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y

  if (y.ts$was.transformed)
  {
    fmla.txt <- "y ~ trend"
  } else
  {
    fmla.txt <- "y ~ trend + season"
  }

  fit <- forecast::tslm(as.formula(fmla.txt), ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  }
  output <- tsm(model.name, fit)
  return(output)
}
