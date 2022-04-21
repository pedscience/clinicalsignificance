#' Calculate slope data for plotting
#'
#' @param intercept Numeric, the intercept
#' @param slope Numeric, the slope
#' @param min Numeric, the first measurement
#' @param max NUmeric, the last measurement
#'
#' @importFrom dplyr tibble
#' @importFrom rlang .data
#'
#' @return A tibble with (max - min) rows and columns `time` and `fitted`
#'
#' @noRd
.calc_slope_data <- function(intercept, slope, min, max) {
  tibble(
    time = seq(min, max),
    fitted = intercept + .data$time * slope
  )
}
