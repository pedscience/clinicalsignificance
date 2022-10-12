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
.generate_rci_data_jt <- function(x, lower_limit = 0, upper_limit = 100) {
  s_diff <- x[["rci"]][[1]]

  tibble(
    pre = c(lower_limit, upper_limit),
    ymin = pre - 1.96 * s_diff,
    ymax = pre + 1.96 * s_diff
  )
}
