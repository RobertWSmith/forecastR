## autoplot.R

#' Automatic plotting for time series objects
#'
#' @param object a \code{ts} or \code{mts} object
#' @param title character. If provided, is passed as an argument to
#'   \code{\link[ggplot2]{ggtitle}}
#' @param ... other arguments
#'
#' @importFrom ggplot2 autoplot ggplot geom_line ggtitle aes_string
#' @importFrom broom tidy
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#'
#' ap <- ts.split(AirPassengers, as.list = FALSE)
#' ##autoplot(ap, title = "Air Passengers Subsetting")

# autoplot.ts <- function(object, title = NULL, ...)
# {
#   obj <- .tidy.ts(object, ...)
#   gg <- ggplot2::ggplot(obj, ggplot2::aes_string(x = "time", y = "value",
#                                                  color = "variable")) +
#     ggplot2::geom_line()
#
#   if (frequency(object) == 12L)
#   {
#     obj$date <- as.Date(ISOdate(as.integer(obj$time), obj$cycle, 1))
#     gg <- ggplot2::ggplot(obj, ggplot2::aes_string(x = "date", y = "value",
#                                                    color = "variable")) +
#       ggplot2::geom_line()
#   }
#
#   if (!is.null(title))
#     gg <- gg + ggplot2::ggtitle(title)
#   print(gg)
#   return(gg)
# }
#
# autoplot.mts <- autoplot.ts.split <- autoplot.ts
