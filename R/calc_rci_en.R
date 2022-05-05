#' RCI for the Edwards method
#'
#' This function expects a data frame with at least columns `pre` and `post`.
#' The reliability must be a value between 0 and 1.
#'
#' @param data A preprocessed data frame
#' @param reliability The instrument's reliability
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @importFrom rlang .data
#'
#' @return A data frame with columns `id`, `pre_true` (adjusted true score of
#'   `pre`), `lower`, `upper`, `improved`, `deteriorated`, and `unchanged`
#'
#' @noRd
.calc_rci_en <- function(data, m_pre, sd_pre, reliability, direction = 1) {
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)


  # Calculate confidence interval around the true pre score (adjusted for
  # regression to the mean)
  confidence_borders <- data %>%
    mutate(
      pre_true = reliability * (.data$pre - m_pre) + m_pre,
      lower = .data$pre_true - 2 * se_measurement,
      upper = .data$pre_true + 2 * se_measurement
    )


  # Does post fall outside this interval?
  if (direction == 1) {
    rci_results <- confidence_borders %>%
      mutate(
        improved = ifelse(.data$post > .data$upper, TRUE, FALSE),
        deteriorated = ifelse(.data$post < .data$lower, TRUE, FALSE),
        unchanged = !.data$improved & !.data$deteriorated
      )
  } else if (direction == -1) {
    rci_results <- confidence_borders %>%
      mutate(
        improved = ifelse(.data$post < .data$lower, TRUE, FALSE),
        deteriorated = ifelse(.data$post > .data$upper, TRUE, FALSE),
        unchanged = !.data$improved & !.data$deteriorated
      )
  }


  data_rci_categories <- rci_results %>%
    select(.data$id, .data$pre_true, .data$lower, .data$upper, .data$improved:.data$unchanged)

  list(
    se_measurement = se_measurement,
    data = data_rci_categories
  )
}
