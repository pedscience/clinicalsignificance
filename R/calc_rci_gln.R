#' RCI for the Gulliksen, Lord & Novick method
#'
#' This function expects a data frame with at least columns `pre` and `post`.
#' The reliability must be a value between 0 and 1.
#'
#' @param data A preprocessed data frame
#' @param reliability Instrument's reliability
#' @param direction Which direction is better? 1 = higher, -1 = lower
#' @param m_pre Mean pre measurement
#' @param sd_pre SD pre measurement
#' @param critical_value The critical value for the RCI decision, should be 1.96
#'
#' @importFrom stats sd
#'
#' @return A vector with RCIs
#'
#' @noRd
.calc_rci_gln <- function(data, m_pre, sd_pre, reliability, direction = 1, critical_value = 1.96) {
  se_prediction <- .calc_se_prediction(sd_pre = sd_pre, reliability = reliability)

  # Calculate RCI
  rci_data <- data |>
    mutate(
      pre_adj = reliability * (pre - m_pre),
      post_adj = post - m_pre,
      change_adj = post_adj - pre_adj,
      rci = change_adj / se_prediction
    )


  # Calculate categories
  data_rci_categories <- .calc_improvement(
    data = rci_data,
    rci_cutoff = critical_value,
    direction = direction
  )

  list(
    se_prediction = se_prediction,
    data = data_rci_categories
  )
}
