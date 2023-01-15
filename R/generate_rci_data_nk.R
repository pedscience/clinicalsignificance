#' Generate data for RCI plotting
#'
#' @param x A clinisig object
#' @param lower_limit Lower plotting limit
#' @param upper_limit Upper plotting limit
#'
#' @return A tibble with columns `pre`, `ymin`, and `ymax`
#'
#' @noRd
.generate_rci_data_nk <- function(x, lower_limit = 0, upper_limit = 100) {
  m_pre <- get_cutoff_descriptives(x)[["m_clinical"]]
  sd_pre <- get_cutoff_descriptives(x)[["sd_clinical"]]
  reliability_pre <- get_reliability(x)[[1]]
  reliability_post <- get_reliability(x)[[2]]

  tibble(
    pre = c(lower_limit, upper_limit),
    ymin = -1.96 * sqrt((reliability_pre^2 * sd_pre^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post))) + (reliability_pre * (pre - m_pre) + m_pre),
    ymax = 1.96 * sqrt((reliability_pre^2 * sd_pre^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post))) + (reliability_pre * (pre - m_pre) + m_pre)
  )
}
