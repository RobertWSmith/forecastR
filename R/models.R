

### template for short time series
# y.ts <- short.ts(y, freq.multiple = 2.25)
# y <- y.ts$y
# fit <- sapply(fit, function(x) {
#   if (is.ts(x))
#     return(short.ts.inv(x, y.ts))
#   return(x)
# })
### template for short time series

# internal function to parse short time series
#' @importFrom stats ts start tsp frequency
short.ts <- function(y, freq.multiple = 2.25) {
  orig.y <- y
  orig.freq <- as.numeric(frequency(y))
  needs.transformed <- ((orig.freq * freq.multiple) > length(y))
  if (needs.transformed)
    y <- ts(as.numeric(y))
  return(list(y = y, freq.multiple = freq.multiple, orig.freq = orig.freq,
              orig.start = start(orig.y), orig.tsp = tsp(orig.y),
              was.transformed = needs.transformed))
}


#' @importFrom stats ts tsp
short.ts.inv <- function(y.short.ts, x.ts = NULL, ...)
{
  if (!is.null(x.ts) && !is.ts(x.ts))
    stop('`x.ts` must be `ts` object.')
  ts.tsp <- y.short.ts$orig.tsp
  if (!is.list(y.short.ts))
    ts.tsp <- tsp(y.short.ts)

  if (is.null(x.ts))
    x.ts <- y.short.ts$y

  ts.start <- ts.tsp[1]
  ts.freq <- ts.tsp[3]

  return(ts(as.numeric(x.ts), start = ts.start, frequency = ts.freq))
}


#' ARIMA model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{Arima}} and
#' \code{\link[forecast]{auto.arima}} to allow unified interface.
#'
#' @param y Univariate Time Series
#' @param ... additional argument for model function
#' @param model.name name of model specialization
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{Arima}} and \code{\link[forecast]{auto.arima}}
#'
#' @importFrom forecast Arima auto.arima
#' @importFrom pryr dots
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' fit <- arima(AirPassengers)
#' fit2 <- arima(AirPassengers, D=1)
#' fit3 <- arima(AirPassengers, lambda=0)
arima <- function(y, ..., model.name="arima")
{
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  kw <- dots(...)
  if (any(c("model", "order") %in% names(kw)))
  {
    fit <- forecast::Arima(y, ...)
  } else
  {
    # overwrite function name to point to model
    fit <- forecast::auto.arima(y, ...)
  }
  fit.cls <- class(fit)
  fit <- sapply(names(fit), function(x) {
    if (x %in% c("residuals", "x", "fitted"))
      return(short.ts.inv(y.ts, as.ts(fit[[x]])))
    else
      return(fit[[x]])
  })
  class(fit) <- fit.cls
  output <- tsm(model.name, fit)
  return(output)
}


#' @export
#' @seealso \code{\link[forecastR]{arima}}
#' @importFrom pryr partial
sarima <- pryr::partial(arima, D=1, model.name="arima")


#' ARFIMA model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{arfima}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param ... additional argument for model function
#' @param model.name name of model specialization
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{arfima}}
#'
#' @importFrom forecast arfima
#' @importFrom pryr dots
#' @importFrom stats is.ts
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' fit <- arfima(AirPassengers)
#' fit2 <- arfima(AirPassengers, lambda=0)
arfima <- function(y, ..., model.name="arfima")
{
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::arfima(y, ...)

  fit.cls <- class(fit)
  fit <- sapply(names(fit), function(x) {
    if (x %in% c("residuals", "x", "fitted"))
      return(short.ts.inv(y.ts, as.ts(fit[[x]])))
    else
      return(fit[[x]])
  })
  class(fit) <- fit.cls

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
#' @param model.name name of model specialization
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
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' fit <- bats(AirPassengers)
#' fit2 <- bats(AirPassengers, lambda=0)
bats <- function(y, ..., model.name="bats")
{
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::bats(y, ...)

  fit.cls <- class(fit)
  fit <- sapply(names(fit), function(x) {
    if (x %in% c("residuals", "fitted", "y", "fitted.values", "errors"))
      return(short.ts.inv(y.ts, as.ts(fit[[x]])))
    else
      return(fit[[x]])
  })
  class(fit) <- fit.cls

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
#' @param model.name name of model specialization
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{ets}}
#'
#' @importFrom forecast ets
#' @importFrom pryr dots
#' @importFrom stats is.ts
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' fit <- ets(AirPassengers)
#' fit2 <- ets(AirPassengers, lambda=0)
ets <- function(y, ..., model.name="ets")
{
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::ets(y, ...)

  fit.cls <- class(fit)
  fit <- sapply(names(fit), function(x) {
    if (x %in% c("residuals", "x", "fitted"))
      return(short.ts.inv(y.ts, as.ts(fit[[x]])))
    else
      return(fit[[x]])
  })
  class(fit) <- fit.cls

  output <- tsm(model.name, fit)
  return(output)
}


#' @export
#' @seealso \code{\link[forecastR]{arima}}
#' @importFrom pryr partial
ets.multiplicative <- pryr::partial(ets, allow.multiplicative.trend = TRUE,
                                    model.name="ets.multiplicative")

#' NNETAR model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{nnetar}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param ... additional argument for model function
#' @param model.name name of model specialization
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{nnetar}}
#'
#' @importFrom forecast nnetar
#' @importFrom pryr dots
#' @importFrom stats frequency tsp tsp<- ts is.ts
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' fit <- nnetar(AirPassengers)
#' fit2 <- nnetar(AirPassengers, lambda=0)
nnetar <- function(y, ..., model.name="nnetar")
{
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::nnetar(y, ...)

  fit.cls <- class(fit)
  fit <- sapply(names(fit), function(x) {
    if (x %in% c("residuals", "x", "fitted"))
      return(short.ts.inv(y.ts, as.ts(fit[[x]])))
    else
      return(fit[[x]])
  })
  class(fit) <- fit.cls

  output <- tsm(model.name, fit)
  return(output)
}



#' @export
#' @seealso \code{\link[forecastR]{arima}}
#' @importFrom pryr partial
nnetar.w.decay <- pryr::partial(nnetar, decay=0.1, model.name="nnetar.w.decay")


#' STLM model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{stlm}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param ... additional argument for model function
#' @param model.name name of model specialization
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
#' data('AirPassengers', package='datasets')
#' fit <- stlm(AirPassengers)
#' fit2 <- stlm(AirPassengers, lambda=0)
stlm <- function(y, ..., model.name="stlm")
{
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  kw <- dots(...)
  if ("model" %in% names(kw)) {
    kw$y <- y
    kw$etsmodel <- kw$model
    kw$model <- NULL
    fit <- do.call(forecast::stlm, kw)
  } else {
    fit <- forecast::stlm(y, ...)
  }

  fit.cls <- class(fit)
  fit <- sapply(names(fit), function(x) {
    if (x %in% c("residuals", "x", "fitted"))
      return(short.ts.inv(y.ts, as.ts(fit[[x]])))
    else
      return(fit[[x]])
  })
  class(fit) <- fit.cls
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
#' @param model.name name of model specialization
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
#' data('AirPassengers', package='datasets')
#' fit <- nnetar(AirPassengers)
#' fit2 <- nnetar(AirPassengers, lambda=0)
tbats <- function(y, ..., model.name="tbats")
{
  y.orig <- y
  y.ts <- short.ts(y, freq.multiple = 2.25)
  y <- y.ts$y
  fit <- forecast::tbats(y, ...)

  fit.cls <- class(fit)
  fit <- sapply(names(fit), function(x) {
    if (x %in% c("errors", "y", "fitted.values"))
      return(short.ts.inv(y.ts, as.ts(fit[[x]])))
    else
      return(fit[[x]])
  })
  class(fit) <- fit.cls

  output <- tsm(model.name, fit)
  return(output)
}


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
forecast <- function(object, ...) {
  return(forecast::forecast(model(object), ...))
}
