#' Generate data for RCI plotting
#'
#' @param x A clinisig object
#' @param lower_limit Lower plotting limit
#' @param upper_limit Upper plotting limit
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
  critical_value <- x[["critical_value"]]

  tibble(
    pre = c(lower_limit, upper_limit),
    ymin = (-critical_value * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd,
    ymax = (critical_value * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd
  )
}


#' Generate data for cut score band plotting for Hagemann and Arrindell
#'
#' @param x A clinisig object
#' @param lower_limit Lower plotting limit
#' @param upper_limit Upper plotting limit
#'
#' @return A tibble with columns `pre`, `ymin`, and `ymax`
#'
#' @noRd
.generate_true_cut_data <- function(x, lower_limit = 0, upper_limit = 100) {
  rel_post <- x[["cutoff"]][["reliability_post"]]
  se_measurement <- x[["rci"]][["se_measurement"]]
  m_post <- x[["cutoff"]][["m_post"]]
  cutoff <- x[["cutoff"]][["info"]][["value"]]
  critical_value <- x[["critical_value"]]

  tibble(
    pre = c(lower_limit, upper_limit),
    ymin = (-critical_value * sqrt(rel_post) * se_measurement - m_post + m_post * rel_post + cutoff) / rel_post,
    ymax = (critical_value * sqrt(rel_post) * se_measurement - m_post + m_post * rel_post + cutoff) / rel_post
  )
}
