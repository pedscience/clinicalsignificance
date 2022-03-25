#' Calculate RCI scores
#'
#' @importFrom stats sd
#'
#' @param data A preprocessed wide dataframe
#' @param reliability The instrument's reliability
prep_rci <- function(data, reliability) {
  sd_pre <- sd(data$pre)
  s_diff <- .calc_s_diff(sd_pre = sd_pre, reliability = reliability)

  data$change / s_diff
}


# RCI for the Jacobson method
prep_rci_jacobson <- function(data, reliability) {
  sd_pre <- sd(data$pre)
  s_diff <- .calc_s_diff(sd_pre = sd_pre, reliability = reliability)

  data$change / s_diff
}


#' Calculate standard error of the instrument
#'
#' @param sd_pre Pre standard deviation
#' @param reliability Instrument's reliability
.calc_s_diff <- function(sd_pre, reliability) {
  s_e <- sd_pre * sqrt(1 - reliability)
  sqrt(2 * s_e^2)
}
