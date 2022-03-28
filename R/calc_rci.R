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
#'
#' @noRd
.calc_rci_jacobson <- function(data, reliability) {
  sd_pre <- sd(data$pre)
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)
  s_diff <- .calc_s_diff(se_measurement)

  data$change / s_diff
}



#' RCI for the Gulliksen method
#'
#' This function expects a data frame with at least columns `pre` and `post`.
#' The reliability must be a value between 0 and 1.
#'
#' @param data A preprocessed data frame
#' @param reliability Instrument's reliability
#'
#' @importFrom stats sd
#'
#' @return A vector with RCIs
#'
#' @noRd
.calc_rci_gulliksen <- function(data, reliability) {
  m_pre <- mean(data$pre)
  sd_pre <- sd(data$pre)
  se_prediction <- .calc_se_prediction(sd_pre = sd_pre, reliability = reliability)


  # Adjustments of scores
  pre_adj <-  reliability * (data$pre - m_pre)
  post_adj <- data$post - m_pre
  change_adj <- post_adj - pre_adj


  # RCI
  change_adj / se_prediction
}


#' Calculate standard error of measurement
#'
#' \deqn{S_{diff} = \frac{x_2 - x_1}{S_E}}
#' \deqn{S_E = SD_{pre} \sqrt{1 - r}}
#' With \eqn{r} being the reliability
#'
#' @param sd_pre Pre measurement standard deviation
#' @param reliability Instrument's reliability
#'
#' @return A number
#'
#' @noRd
.calc_se_measurement <- function(sd_pre, reliability) {
  sd_pre * sqrt(1 - reliability)
}


#' Calculate standard error of differences
#'
#' @param se_measurement A standard error of measurements
#'
#' @return A number
#'
#' @noRd
.calc_s_diff <- function(se_measurement) {
  sqrt(2 * se_measurement^2)
}


#' Calculate standard error of predictions
#'
#' @param sd_pre Pre measurement standard deviation
#' @param reliability Instrument's reliability
#'
#' @return A number
#'
#' @noRd
.calc_se_prediction <- function(sd_pre, reliability) {
  sd_pre * sqrt(1 - reliability^2)
}
