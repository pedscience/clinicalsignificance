#' Generate data for RCI plotting
#'
#' @param x A clinisig object
#' @param lower_limit Lower plotting limit
#' @param upper_limit Upper plotting limit
#'
#' @importFrom rlang .data
#'
#' @return A tibble with columns `pre`, `ymin`, and `ymax`
#'
#' @noRd
.generate_rci_data_en <- function(x, lower_limit = 0, upper_limit = 100) {
  se_measurement <- x[["rci"]][[1]]
  reliability <- get_reliability(x)[[1]]
  m_pre <- get_descriptives(x)[["m_clinical"]]

  tibble(
    pre = lower_limit:upper_limit,
    pre_true = reliability * (.data$pre - m_pre) + m_pre,
    ymin = .data$pre_true - 2 * se_measurement,
    ymax = .data$pre_true + 2 * se_measurement
  )
}
