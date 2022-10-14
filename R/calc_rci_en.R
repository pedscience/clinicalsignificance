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
#'   `pre`), `improved`, `deteriorated`, and `unchanged`
#'
#' @noRd
.calc_rci_en <- function(data, m_pre, sd_pre, reliability, direction = 1) {
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)


  # Calculate the RCI according to reformulation of Speer
  rci_data <- data %>%
    mutate(
      pre_true = reliability * (pre - m_pre) + m_pre,
      change_adj = post - pre_true,
      rci = change_adj / se_measurement
    )


  # Calculate categories
  data_rci_categories <- .calc_improvement(
    data = rci_data,
    rci_cutoff = 2,
    direction = direction
  )

  list(
    se_measurement = se_measurement,
    data = data_rci_categories
  )
}
