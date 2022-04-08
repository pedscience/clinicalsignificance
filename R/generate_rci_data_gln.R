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
.generate_rci_data_gln <- function(x, lower_limit = 0, upper_limit = 100) {
  s_prediction <- x[["rci"]][[1]]
  reliability <- get_reliability(x)[[1]]
  m_pre <- get_cutoff_descriptives(x)[["m_clinical"]]

  tibble(
    pre = lower_limit:upper_limit,
    ymin = reliability * .data$pre - reliability * m_pre + m_pre + (-1.96 * s_prediction),
    ymax = reliability * .data$pre - reliability * m_pre + m_pre + (1.96 * s_prediction)
  )
}
