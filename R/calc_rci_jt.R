#' RCI for the Jacobson & Truax method
#'
#' This function expects a data frame with at least column `change`.
#' The reliability must be a value between 0 and 1.
#'
#' @param data A preprocessed dataframe
#' @param reliability Instrument's reliability
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @importFrom stats sd
#' @importFrom rlang .data
#'
#' @return A vector with RCIs
#'
#' @noRd
.calc_rci_jt <- function(data, sd_pre, reliability, direction = 1) {
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)
  s_diff <- .calc_s_diff(se_measurement)


  # Calculate RCI
  rci_data <- data %>%
    mutate(
      rci = .data$change / s_diff
    )


  # Caluclate categories
  data_with_rci <- .calc_improvement(
    data = rci_data,
    rci_cutoff = 1.96,
    direction = direction
  )

  list(
    s_diff = s_diff,
    data = data_with_rci
  )
}
