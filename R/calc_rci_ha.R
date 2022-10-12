#' RCI for the Hageman & Arrindell
#'
#' This function expects a data frame with at least columns `pre` and `change`.
#' The reliability must be a value between 0 and 1.
#'
#' @param data A preprocessed dataframe
#' @param reliability Instrument's reliability
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @importFrom stats sd cor
#'
#' @return A vector with RCIs
#'
#' @noRd
.calc_rci_ha <- function(data, m_pre, sd_pre, m_post, sd_post, reliability, direction = 1) {
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)
  r_xx_1 <- .calc_reliability_ha(sd = sd_pre, se_measurment = se_measurement)
  r_xx_2 <- .calc_reliability_ha(sd = sd_post, se_measurment = se_measurement)
  cor_pre_post <- cor(data[["pre"]], data[["post"]])

  nominator <- (sd_pre^2 * r_xx_1 + sd_post^2 * r_xx_2 - 2 * sd_pre * sd_post * cor_pre_post)
  denominator <- (sd_pre^2 + sd_post^2 - 2 * sd_pre * sd_post * cor_pre_post)

  r_dd <- nominator / denominator

  rci_data <- data %>%
    mutate(
      rci = ((post - pre) * r_dd + (m_post - m_pre) * (1 - r_dd)) / (sqrt(r_dd) * sqrt(2 * se_measurement^2))
    )

  # Caluclate categories
  data_with_rci <- .calc_improvement(
    data = rci_data,
    rci_cutoff = 1.65,
    direction = direction
  )

  list(
    r_dd = r_dd,
    se_measurement = se_measurement,
    data = data_with_rci
  )
}
