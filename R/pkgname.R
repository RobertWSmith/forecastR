## pkgname.R

#' forecastR
#'
#' This package relies heavily on \code{\link{forecast}} and \code{\link{opera}}
#' to help forecasters select and build high-quality forecasts.
#'
#' @name forecastR-package
#' @aliases forecastR-package forecastR
#' @docType package
#' @author Robert Smith <rob_smith@goodyear.com>
#'
#' @keywords package
NULL


## Tidy dir settings.


# formatR::tidy_dir(path = '.', recursive = TRUE, comment =
# TRUE, blank = TRUE, arrow = TRUE, brace.newline = TRUE,
# indent = 2, output = FALSE, text = NULL, width.cutoff = 60)


#' @importFrom settings options_manager inlist inrange
PACKAGE_OPTIONS <- settings::options_manager(
  autofit.models = c("arfima", "arima", "bats", "ets", "nnetar", "stlm", "tbats", "tslm"),
  autofit.models.short.ts = c("arima", "ets"),
  short.ts.frequency.multiple = 2.25,
  ts.fields = list(
    arima = c("residuals", "x"),
    arfima = c("residuals", "x", "fitted"),
    bats = c("errors", "y", "fitted.values"),
    ets = c("residuals", "x", "fitted"),
    nnetar = c("residuals", "x", "fitted"),
    stlm = c("residuals", "x", "fitted"),
    tbats = c("errors", "y", "fitted.values"),
    tslm = c("residuals", "fitted.values"),
    forecast = c("mean", "lower", "upper", "x", "fitted", "residuals")
    )
  )

# , .allowed = list( autofit.models =
# settings::inlist('arima', 'arfima', 'bats', 'ets',
# 'nnetar', 'stlm', 'tbats', 'tslm'), autofit.models.short.ts
# = settings::inlist('arima', 'ets'),
# short.ts.frequency.multiple = settings::inrange(min = 1,
# max = Inf) )


#' Set or get options for \code{forecastR} package
#'
#' @param ... Option names to retrieve option values or \code{[key] = [value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{models}}{
#'    \itemize{
#'      \item{\code{arima}}{ARIMA model will be considered, see also \code{\link[forecast]{Arima}} & \code{\link[forecast]{auto.arima}}}
#'      \item{\code{arfima}}{ARFIMA model will be considered, see also \code{\link[forecast]{arfima}}}
#'      \item{\code{bats}}{BATS models will be considered, see also \code{\link[forecast]{bats}}}
#'      \item{\code{ets}}{ETS model will be considered, see also \code{\link[forecast]{ets}}}
#'      \item{\code{nnetar}}{NNETAR model will be considered, see also \code{\link[forecast]{nnetar}}}
#'      \item{\code{stlm}}{STLM model will be considered, see also \code{\link[forecast]{stlm}}}
#'      \item{\code{tbats}}{TBATS model will be considered, see also \code{\link[forecast]{tbats}}}
#'      \item{\code{tslm}}{TSLM model will be considered, see also \code{\link[forecast]{tslm}}}
#'    }
#'  }
#' }
#'
#' @importFrom settings stop_if_reserved options_manager inlist
#'
#' @export
package_options <- function(...)
{
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  return(PACKAGE_OPTIONS(...))
}


#' Reset global options for pkg
#'
#' @importFrom settings reset
#'
#' @export
pkg_reset <- function()
{
  settings::reset(PACKAGE_OPTIONS)
}


.onLoad <- function(libname, pkgname)
{
  pkg_reset()
}
