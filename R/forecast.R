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
forecast <- function(object, ...) {
  return(forecast::forecast(model(object), ...))
}
