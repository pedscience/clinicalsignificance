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
.generate_rci_data_ha <- function(x, lower_limit = 0, upper_limit = 100) {
  r_dd <- x[["rci"]][[1]]
  se_measurement <- x[["rci"]][[2]]
  data <- get_augmented_data(x)
  m_pre <- mean(data[["pre"]])
  m_post <- mean(data[["post"]])

  tibble(
    pre = lower_limit:upper_limit,
    ymin = (-1.65 * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd,
    ymax = (1.65 * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd
  )
}
