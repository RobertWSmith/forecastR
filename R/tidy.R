## tidy.R

#' Automatic conversion of \code{ts} and \code{mts} objects to \code{data.frame}
#'
#' @param x a \code{ts} or \code{mts} object
#' @param na.rm logical. If \code{TRUE}, when numeric values are coalesced,
#'   missing observations are dropped, otherwise they are kept as \code{NA}
#' @param variable.name character. Name of column which indicates the source of
#'   the values in `value.name`
#' @param value.name character. Name of the resulting column of values.
#' @param ... other arguments
#'
#' @importFrom broom tidy
#' @importFrom reshape2 melt
#'
#' @examples
#' library(broom)
#' library(forecastR)
#' ap <- ts.split(AirPassengers)
#' head(ap)
#'
#' tidy.ap <- tidy(ap)
#' head(tidy.ap)
.tidy.ts <- function(x, na.rm=TRUE, variable.name = "variable",
                    value.name = "value", ...)
{
  obj <- as.data.frame(x)
  colnames(obj) <- ifelse(inherits(x, "mts"), colnames(x), value.name)

  obj$time <- as.numeric(time(x))
  obj$cycle <- as.integer(cycle(x))

  obj <- reshape2::melt(obj, id.vars=c("time", "cycle"), na.rm = na.rm,
                        variable.name = variable.name, value.name = value.name)
  return(obj)
}

tidy.ts <- .tidy.ts


#' Tidy \code{ts.split} object into \code{data.frame}.
#'
#' @param x \code{ts.split} object
#' @param ... arguments passed on to \code{\link[base]{as.data.frame}}
#'
#' @return \code{\link{data.frame}} with `time` column and columns for
#'   each \code{\link[stats]{ts}} object. If \code{\link[stats]{ts}} inherits
#'   from \code{mts}, \code{\link{matrix}} or is a
#'   \code{\link{list}} of \code{\link[stats]{ts}} objects, it is coerced
#'   to a \code{mts} before using the native call to
#'   \code{\link[base]{as.data.frame}}.
#'
#' @export
#'
#' @importFrom stats time cycle frequency deltat
#' @importFrom broom tidy
#'
#' @seealso \code{\link[broom]{tidy}} \code{\link[base]{as.data.frame}}
#'
#' @examples
#' library(forecastR)
#' library(broom)
#' x <- ts(1:100, freq=12)
#' x.split <- ts.split(x)
#' x.tidy <- tidy(x.split)
#' is.data.frame(x.tidy)
#' head(x.tidy)
tidy.ts.split <- function(x, ...)
{
  if (is.list(x))
    x <- do.call(cbind, x)

  x <- as.ts(x)
  if (is.matrix(x))
    class(x) <- c("mts", "ts", "matrix")
  else
    class(x) <- "ts"
  return(broom::tidy(x, ...))
}

