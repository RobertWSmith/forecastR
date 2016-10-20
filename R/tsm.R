

#' Wrapper for Time Series Models that creates a unified interface.
#'
#' Internal function which applies \code{tsm} object formatting to time series
#' models.
#'
#' @param function.name function name for the model
#' @param y fitted time series model
#' @param ... not currently used
#'
#' @return object of class \code{tsm}
#'
#' @include models.R
tsm <- function(function.name, y, ...) UseMethod("tsm")


#' @importFrom pryr dots
#' @export
tsm.default <- function(function.name, y, ...)
{
  output <- structure(list(function.name = function.name, model = y), class = "tsm")
  return(output)
}


#' @export
summary.tsm <- function(object, ...)
{
  return(summary(model(object, ...)))
}


#' @importFrom stats residuals
#' @export
residuals.tsm <- function(object, ...)
{
  return(residuals(model(object, ...)))
}


#' @importFrom stats coef
#' @export
coef.tsm <- function(object, ...)
{
  return(coef(model(object, ...)))
}


#' Extract a model from an object
#'
#' @param object object to be evaluated
#' @param ... additional arguments for class specific evaluation
#'
#' @export
#'
#' @return model object
model <- function(object, ...)
  UseMethod("model")

# pass through function for non-tsm objects
#' @export
model.default <- function(object, ...)
{
  return(object)
}

#' @export
model.tsm <- function(object, ...)
{
  return(object$model)
}

#' @export
`model<-` <- function(object, value)
  UseMethod("model<-")

#' @export
`model<-.tsm` <- function(object, value)
{
  object$model <- value
  return(object)
}

