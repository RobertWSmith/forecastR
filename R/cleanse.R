## cleanse.R

#' Cleanse Time Series in preparation for fitting
#'
#' @param x \code{numeric}
#' @param ... additional arguments
#'
#' @export
cleanse <- function(x, ...)
{
  shift <- 1.0
  if (min(x) < shift)
    shift <- abs(min(x)) + shift

  x.adj <- x + shift

  attr(x.adj, "shift") <- shift
  return(x.adj)
}


#' Invert cleansed time series
#'
#' @param x univariate time series
#' @param cleansed.x cleansed attr
#' @param ... additional arguments
#'
#' @export
inv.cleanse <- function(x, cleansed.x = NULL, ...)
{
  shift <- 1.0
  if (!is.null(cleansed.x))
    ttr <- cleanse.attributes(cleansed.x)
  else
    ttr <- cleanse.attributes(x)

  if (!is.null(ttr$shift))
    shift <- ttr$shift

  x <- x - shift
  attr(x, "shift") <- NULL
  return(x)
}


#' Extract cleanse attributes from time series
#'
#' @param x univariate time series
#' @param ... additional arguments
#'
#' @export
cleanse.attributes <- function(x, ...)
{
  tmp <- list()
  if (!is.null(x))
    tmp <- attributes(x)

  if (is.list(x))
    tmp <- x

  if (is.null(tmp$shift))
    tmp$shift <- 0.0

  return(tmp)
}

cleanse.attr <- cleanse.attributes
