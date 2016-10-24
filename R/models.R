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
#' @param d integer. Number of non seasonal differences. See \code{\link[forecast]{auto.arima}}
#' @param D integer. Number of seasonal differences. See \code{\link[forecast]{auto.arima}}
#' @param stationary logical. Passed to \code{\link[forecast]{auto.arima}} if
#'   \code{model} or \code{order} argument not passed as keyword arguments.
#' @param seasonal logical. Defualt determined by checking length of time series `y`,
#'   if using monthly data (frequency = 12), `y` must have more than 27 records to
#'   fit a seasonal model.
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
#' @importFrom pryr dots
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
arima <- function(y, d = NA, D = NA, stationary = TRUE, seasonal = (length(y) >
  2.25 * frequency(y)), model = NULL, order = NULL, ...)
  {
  model.name <- "arima"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  kw <- dots(...)
  if (!is.null(model))
  {
    mdl <- model(model)
    fit <- forecast::Arima(y, model = mdl, ...)
  } else if (!is.null(order))
  {
    fit <- forecast::Arima(y, order = order, ...)
  } else
  {
    D <- ifelse(seasonal, 1, 0)
    fit <- forecast::auto.arima(y, d = d, D = D, stationary = stationary,
      seasonal = seasonal, ...)
  }
  for (nm in package_options("ts.fields")[model.name][[1]]) fit[[nm]] <- short.ts.inv(y.ts,
    as.ts(fit[[nm]]))
  output <- tsm(model.name, fit)
  return(output)
}


#' ARFIMA model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{arfima}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{arfima}}
#'
#' @importFrom forecast arfima
#' @importFrom pryr dots
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
arfima <- function(y, ...)
{
  model.name <- "arfima"
  kw <- pryr::dots(...)
  kw$model <- NULL
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  kw$y <- quote(y)
  fit <- do.call(forecast::arfima, kw)
  for (nm in package_options("ts.fields")[model.name][[1]])
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  output <- tsm(model.name, fit)
  return(output)
}


#' BATS model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{bats}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{bats}}
#'
#' @importFrom forecast bats
#' @importFrom pryr dots
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
bats <- function(y, ...)
{
  model.name <- "bats"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::bats(y, ...)
  for (nm in package_options("ts.fields")[model.name][[1]])
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  output <- tsm(model.name, fit)
  return(output)
}


#' ETS model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{ets}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{ets}}
#'
#' @importFrom forecast ets
#' @importFrom pryr dots
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
ets <- function(y, ...)
{
  model.name <- "ets"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  kw <- pryr::dots(...)
  kw$allow.multiplicative.trend <- TRUE
  kw$y <- quote(y)
  fit <- do.call(forecast::ets, kw)
  for (nm in package_options("ts.fields")[model.name][[1]])
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  output <- tsm(model.name, fit)
  return(output)
}


#' NNETAR model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{nnetar}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{nnetar}} \code{\link[forecastR]{nnetar.w.decay}}
#'
#' @importFrom forecast nnetar
#' @importFrom pryr dots
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
nnetar <- function(y, ...)
{
  model.name <- "nnetar"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::nnetar(y, ...)
  for (nm in package_options("ts.fields")[model.name][[1]])
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  output <- tsm(model.name, fit)
  return(output)
}

#' @describeIn nnetar NNETAR w/ default decay
#' @importFrom pryr partial
#' @export
nnetar.w.decay <- pryr::partial(nnetar, decay = 0.01, model.name = "nnetar.w.decay")


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
#' @seealso \code{\link[forecast]{tbats}}
#'
#' @importFrom forecast tbats
#' @importFrom pryr dots
#' @importFrom stats is.ts
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- stlm(AirPassengers)
#' fit2 <- stlm(AirPassengers, lambda = 0)
stlm <- function(y, model = NULL, ...)
{
  model.name <- "stlm"
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  kw <- dots(...)
  if ("model" %in% names(kw))
  {
    kw$y <- y
    kw$etsmodel <- kw$model
    kw$model <- NULL
    fit <- do.call(forecast::stlm, kw)
  } else
  {
    fit <- forecast::stlm(y, ...)
  }
  for (nm in package_options("ts.fields")[model.name][[1]])
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  output <- tsm(model.name, fit)
  return(output)
}


#' TBATS model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{tbats}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{tbats}}
#'
#' @importFrom forecast tbats
#' @importFrom pryr dots
#' @importFrom stats is.ts
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- nnetar(AirPassengers)
#' fit2 <- nnetar(AirPassengers, lambda = 0)
tbats <- function(y, ...)
{
  model.name <- "tbats"
  y.orig <- y
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::tbats(y, ...)
  for (nm in package_options("ts.fields")[model.name][[1]])
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
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
#' @importFrom pryr dots
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
  kw <- dots(...)
  y.orig <- y
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fmla.txt <- "y ~ trend + season"
  if (y.ts$was.transformed)
    fmla.txt <- "y ~ trend"
  fit <- forecast::tslm(as.formula(fmla.txt), ...)
  for (nm in package_options("ts.fields")[model.name][[1]])
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]))
  output <- tsm(model.name, fit)
  return(output)
}
