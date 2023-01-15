#' RCI for the NK method
#'
#' This function expects at least a data frame with columns `pre` and `post`.
#' Reliability must be between 0 and 1.
#'
#' @param data A preprocessed data frame
#' @param m_pre Mean of pre measurement
#' @param sd_pre SD of pre measurement
#' @param reliability_pre Instrument' reliability pre
#' @param reliability_post Instrument's reliability post
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#'
#' @return A list with RCI info and data
#'
#' @noRd
.calc_rci_nk <- function(data, m_pre, sd_pre, reliability_pre, reliability_post, direction = 1) {
  denominator <- sqrt((reliability_pre^2 * sd_pre ^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post)))

  rci_data <- data |>
    mutate(
      pre_adj = reliability_pre * (pre - m_pre) + m_pre,
      change_adj = post - pre_adj,
      rci = change_adj / denominator
    )

  data_rci_categories <- .calc_improvement(
    data = rci_data,
    rci_cutoff = 1.96,
    direction = direction
  )

  list(
    reliability_post = reliability_post,
    data = data_rci_categories
  )
}
