#' RCI for the Jacobson method
#'
#' This function expects a data frame with at least columns `pre` and `change`.
#' The reliability must be a value between 0 and 1.
#'
#' @param data A preprocessed dataframe
#' @param reliability Instrument's reliability
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @importFrom stats sd
#'
#' @return A vector with RCIs
#'
#' @noRd
.calc_rci_jacobson <- function(data, reliability, direction = 1) {
  sd_pre <- sd(data$pre)
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)
  s_diff <- .calc_s_diff(se_measurement)

  data %>%
    mutate(
      rci = change / s_diff
    ) %>%
    .calc_improvement(
      data = .,
      rci_cutoff = 1.96,
      direction = direction
    )
}


#' RCI for the Gulliksen method
#'
#' This function expects a data frame with at least columns `pre` and `post`.
#' The reliability must be a value between 0 and 1.
#'
#' @param data A preprocessed data frame
#' @param reliability Instrument's reliability
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @importFrom stats sd
#'
#' @return A vector with RCIs
#'
#' @noRd
.calc_rci_gulliksen <- function(data, reliability, direction = 1) {
  m_pre <- mean(data$pre)
  sd_pre <- sd(data$pre)
  se_prediction <- .calc_se_prediction(sd_pre = sd_pre, reliability = reliability)

  data %>%
    mutate(
      pre_adj = reliability * (data$pre - m_pre),
      post_adj = data$post - m_pre,
      change_adj = post_adj - pre_adj,
      rci = change_adj / se_prediction
    ) %>%
    .calc_improvement(
      data = .,
      rci_cutoff = 1.96,
      direction = direction
    )
}


#' RCI for the Edwards method
#'
#' This function expects a data frame with at least columns `pre` and `post`.
#' The reliability must be a value between 0 and 1.
#'
#' @param data A preprocessed data frame
#' @param reliability The instrument's reliability
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @return A data frame with columns `id`, `pre_true` (adjusted true score of
#'   `pre`), `lower`, `upper`, `improved`, `deteriorated`, and `unchanged`
#'
#' @noRd
.calc_rci_edwards <- function(data, reliability, direction = 1) {
  m_pre <- mean(data$pre)
  sd_pre <- sd(data$pre)
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)

  confidence_borders <- data %>%
    mutate(
      pre_true = reliability * (pre - m_pre) + m_pre,
      lower = pre_true - 2 * se_measurement,
      upper = pre_true + 2 * se_measurement
    )

  if (direction == 1) {
    rci_results <- confidence_borders %>%
      mutate(
        improved = ifelse(post > upper, TRUE, FALSE),
        deteriorated = ifelse(post < lower, TRUE, FALSE),
        unchanged = !improved & !deteriorated
      )
  } else if (direction == -1) {
    rci_results <- confidence_borders %>%
      mutate(
        improved = ifelse(post < lower, TRUE, FALSE),
        deteriorated = ifelse(post > upper, TRUE, FALSE),
        unchanged = !improved & !deteriorated
      )
  }

  rci_results %>%
    select(id, pre_true, lower, upper, improved:unchanged)
}


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
#' @return A tibble with columns `id`, `rci`, `improved`, `deteriorated`, and
#'   `unchanged`
#'
#' @noRd
.calc_improvement <- function(data, rci_cutoff = 1.96, direction = 1) {
  data %>%
    mutate(
      improved        = ifelse(direction * rci > rci_cutoff, TRUE, FALSE),
      deteriorated    = ifelse(direction * rci < -rci_cutoff, TRUE, FALSE),
      unchanged       = !.data$improved & !.data$deteriorated
    ) %>%
    select(id, rci, improved:unchanged)
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
