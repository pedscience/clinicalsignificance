#' RCI for the Gulliksen, Lord & Novick method
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
.calc_rci_hll <- function(data, m_pre, sd_pre, m_post, reliability, direction = 1) {
  se_prediction <- .calc_se_prediction(sd_pre = sd_pre, reliability = reliability)


  # Calculate RCI
  rci_data <- data |>
    mutate(
      pre_adj = reliability * (pre - m_pre),
      post_adj = post - m_post,
      change_adj = post_adj - pre_adj,
      rci = change_adj / se_prediction
    )


  # Calculate categories
  data_rci_categories <- .calc_improvement(
    data = rci_data,
    rci_cutoff = 1.96,
    direction = direction
  )

  list(
    se_prediction = se_prediction,
    m_post = m_post,
    data = data_rci_categories
  )
}
