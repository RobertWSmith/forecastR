## autoplot.R

#' Automatic plotting for time series objects
#'
#' @param object a \code{ts} or \code{mts} object
#' @param title character. If provided, is passed as an argument to
#'   \code{\link[ggplot2]{ggtitle}}
#' @param ... other arguments
#'
#' @importFrom ggplot2 autoplot ggplot geom_line ggtitle aes
#' @importFrom broom tidy
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#'
#' ap <- ts.split(AirPassengers, as.list = FALSE)
#' autoplot(ap, title = "Air Passengers Subsetting")
autoplot.ts <- function(object, title = NULL, ...)
{
  obj <- tidy.ts(object, ...)
  if (frequency(object) == 12L)
  {
    obj$date <- as.Date(ISOdate(as.integer(obj$time), obj$cycle, 1))
    gg <- ggplot(obj, aes(x = date, y = value, color = variable)) + geom_line()
  } else
  {
    gg <- ggplot(obj, aes(x = time, y = value, color = variable)) + geom_line()
  }

  if (!is.null(title))
    gg <- ggtitle(title)

  return(gg)
}
