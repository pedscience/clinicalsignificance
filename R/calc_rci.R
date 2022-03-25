#' RCI for the Jacobson method
#'
#' This function expects a data frame with at least columns `pre` and `change`.
#' The reliability must be a value between 0 and 1.
#'
#' @param data A preprocessed dataframe
#' @param reliability Instrument's reliability
#'
#' @importFrom stats sd
#'
#' @return A vector with RCIs
.calc_rci_jacobson <- function(data, reliability) {
  sd_pre <- sd(data$pre)
  s_diff <- .calc_s_diff(sd_pre = sd_pre, reliability = reliability)

  data$change / s_diff
}


#' Calculate standard error of the instrument
#'
#' \deqn{S_{diff} = \frac{x_2 - x_1}{S_E}}
#' \deqn{S_E = SD_{pre} \sqrt{1 - r}}
#' With \eqn{r} being the reliability
#'
#' @param sd_pre Pre standard deviation
#' @param reliability Instrument's reliability
#'
#' @return A number
.calc_s_diff <- function(sd_pre, reliability) {
  s_e <- sd_pre * sqrt(1 - reliability)
  sqrt(2 * s_e^2)
}
