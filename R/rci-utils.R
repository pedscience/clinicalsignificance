#' Calculate improvement or deterioration based on RCI scores
#'
#' This functions expects at least a preprocessed data frame with the column
#' `rci`
#'
#' @param data A preprocessed data frame
#' @param rci_cutoff A multiplier for the RCI that is used to calculate the
#'   cutoff
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @importFrom rlang .data
#'
#' @return A tibble with columns `id`, `rci`, `improved`, `deteriorated`, and
#'   `unchanged`
#'
#' @noRd
.calc_improvement <- function(data, rci_cutoff = 1.96, direction = 1) {
  data %>%
    mutate(
      improved        = ifelse(direction * .data$rci > rci_cutoff, TRUE, FALSE),
      deteriorated    = ifelse(direction * .data$rci < -rci_cutoff, TRUE, FALSE),
      unchanged       = !.data$improved & !.data$deteriorated
    ) %>%
    select(.data$id, .data$rci, .data$improved:.data$unchanged)
}


#' Calculate standard error of measurement
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
