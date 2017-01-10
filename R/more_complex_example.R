library(forecastR)
library(ggplot2)

# quarterly revenue data, with multiple metrics
data("freeny")

## for information on this dataset:
# help("freeny")

fn <- list()
fn.start <- as.numeric(row.names(freeny)[1])

fn.output <- list()

for (cn in colnames(freeny))
{
  fn[[cn]] <- ts(freeny[, cn], start = fn.start, frequency = 4L)

  ## this dataset has features which generate warnings, these warnings don't
  ## have any effect on the forecast process, just elimiate certain models
  fits <- suppressWarnings( generate.forecast(fn[[cn]]) )

  test.fcst <- cbind(
    fn[[cn]],
    fits$forecast,
    fits$forecast[, fits$selected.forecast.name]
  )
  colnames(test.fcst) <- as.character(
    c(
      cn,
      colnames(fits$forecast),
      "selected.forecast"
    )
  )

  fn.output[[cn]] <- test.fcst
}
