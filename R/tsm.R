## tsm.R
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
#' @family TimeSeriesModel tsm
#' @export
tsm <- function(function.name, y, ...)
{
  output <- structure(list(function.name = function.name, model = y, ...),
    class = "tsm")
  return(output)
}

#' @param object value to be validated as `tsm` object
#'
#' @return logical value
#'
#' @describeIn tsm Inheritance check for \code{tsm} object
#' @seealso \code{\link[methods]{is}}
#'
#' @importFrom methods is
#'
#' @family TimeSeriesModel tsm
#'
#' @export
is.tsm <- function(object)
{
  return(inherits(object, "tsm"))
}


#' Object Summary for \code{tsm} objects
#'
#' @param object an object for which a summary is desired.
#' @param ... additional arguments affecting the summary produced.
#'
#' @seealso \code{\link[base]{summary}}
#'
#' @export
summary.tsm <- function(object, ...)
{
  return(summary(model(object), ...))
}


#' Extract Model Residuals
#'
#' @param object an object for which the extraction of model residuals is meaningful.
#' @param ... other arguments
#'
#' @return Residuals extracted from object \code{object}.
#'
#' @seealso \code{\link[stats]{residuals}}
#'
#' @importFrom stats residuals
#'
#' @export
residuals.tsm <- function(object, ...)
{
  return(residuals(model(object), ...))
}


#' Extract Model Fitted Values
#'
#' @param object an object for which the extraction of model fitted values is meaningful.
#' @param ... other arguments
#'
#' @importFrom stats fitted
#' @seealso \code{\link[stats]{fitted}}
#'
#' @export
#' @importFrom stats fitted
fitted.tsm <- function(object, ...)
{
  return(fitted(model(object)))
}


#' Extract Model Coefficients
#'
#' @param object an object for which the extraction of model coefficients is meaningful.
#' @param ... other arguments
#'
#' @seealso \code{\link[stats]{coef}}
#' @importFrom stats coef
#'
#' @export
coef.tsm <- function(object, ...)
{
  return(coef(model(object), ...))
}


#' Extract Model from \code{object}
#'
#' @param object an object which stores a model which requires direct access.
#' @param ... other arguments
#'
#' @export
model <- function(object, ...) UseMethod("model")


#' @describeIn model Default model extractor as pass-through
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


#' Time Series Model Assignnment
#'
#' @param object object which is to store a time series model
#' @param value time series model to be stored
#'
#' @export
`model<-` <- function(object, value) UseMethod("model<-")


#' @export
#' @describeIn model<- \code{tsm} object model assignment
`model<-.tsm` <- function(object, value)
{
  object$model <- value
  return(object)
}
