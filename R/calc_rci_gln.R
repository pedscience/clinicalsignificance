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
#' @importFrom rlang .data
#'
#' @return A vector with RCIs
#'
#' @noRd
.calc_rci_gln <- function(data, reliability, direction = 1) {
  m_pre <- mean(data$pre)
  sd_pre <- sd(data$pre)
  se_prediction <- .calc_se_prediction(sd_pre = sd_pre, reliability = reliability)


  # Calculate RCI
  rci_data <- data %>%
    mutate(
      pre_adj = reliability * (.data$pre - m_pre),
      post_adj = .data$post - m_pre,
      change_adj = .data$post_adj - .data$pre_adj,
      rci = .data$change_adj / se_prediction
    )


  # Calculate categories
  rci_results <- .calc_improvement(
    data = rci_data,
    rci_cutoff = 1.96,
    direction = direction
  )

  list(
    se_prediction = se_prediction,
    data = rci_results
  )
}
