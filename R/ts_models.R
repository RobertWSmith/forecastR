## ts_models.R



#' ARFIMA model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{arfima}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param model \code{tsm} object, used to refit model
#' @param lambda optional, if null ignored otherwise treated as the Box-Cox
#'   parameter
#' @param stepwise logical. should arma coefficients be fit by stepwise selection
#'   or by checking all possible models? FALSE indicates all possible models.
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
arfima <- function(y, model = NULL, lambda = NULL, stepwise = FALSE, ...)
{
  model.name <- "arfima"
  kw <- list(...)
  if(!is.null(lambda) && is.na(lambda))
    lambda <- NULL

  y.ts <- short.ts(y, freq.multiple = 2, lambda = lambda)
  y <- y.ts$y

  if (!is.null(model))
    lambda <- model(model)$lambda

  suppressWarnings(
    fit <- forecast::arfima(y, stepwise = stepwise, lambda = lambda, ...)
    )
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]), nm)
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
  kw <- list(...)
  if(!is.null(lambda) && is.na(lambda))
    lambda <- NULL

  y.ts <- short.ts(y, freq.multiple = 2, NULL)
  y <- y.ts$y
  fit <- forecast::bats(y, model = model(model), ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]), nm)
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
#' @param lambda optional, if null ignored otherwise treated as the Box-Cox
#'   parameter
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
stlm <- function(y, model = "ZZN", lambda = NULL, ...)
{
  model.name <- "stlm"
  kw <- list(...)
  if(!is.null(lambda) && is.na(lambda))
    lambda <- NULL

  y.ts <- short.ts(y, freq.multiple = 2, lambda = lambda)
  y <- y.ts$y
  fit <- forecast::stlm(y, lambda = lambda, ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]), nm)
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
  kw <- list(...)
  if(!is.null(lambda) && is.na(lambda))
    lambda <- NULL
  y.orig <- y
  y.ts <- short.ts(y, freq.multiple = 2, lambda = lambda)
  y <- y.ts$y
  fit <- forecast::tbats(y, model = model(model), ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]), nm)
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
#' @param lambda optional, if null ignored otherwise treated as the Box-Cox
#'   parameter
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
tslm <- function(y, model = NULL, lambda = NULL, ...)
{
  model.name <- "tslm"
  kw <- list(...)
  if(!is.null(lambda) && is.na(lambda))
    lambda <- NULL

  y.orig <- y
  y.ts <- short.ts(y, freq.multiple = 2, lambda = lambda)
  y <- y.ts$y

  if (y.ts$was.transformed || (frequency(y) == 1) || (length(y) > frequency(y)))
    fmla <- quote(y ~ trend)
  else
    fmla <- quote(y ~ trend + season)

  fit <- forecast::tslm(fmla, data=as.data.frame(y), lambda = lambda, ...)
  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]), nm)
  }
  output <- tsm(model.name, fit)
  return(output)
}

