#' Calculate Categories based on an RCI
#'
#' @param data A preprocessed data frame
#' @param cutoff A list containing cutoff information
#' @param rci A vector containing RCIs
#' @param rci_cutoff A numveric value, the multiplier according to which a
#'   patient is considered reliably changed
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @inheritParams .calc_cutoff
#'
#' @importFrom rlang .data
#' @importFrom dplyr relocate left_join
#'
#' @return The full data frame with categories
#'
#' @noRd
.calc_recovered <- function(cutoff_data, rci_data) {
  rci_data %>%
    left_join(cutoff_data, by = "id") %>%
    mutate(
      recovered = .data$clinical_pre & .data$functional_post & .data$improved
    ) %>%
    relocate(.data$clinical_pre, .data$functional_post, .data$recovered, .before = .data$improved) %>%
    relocate(.data$unchanged, .after = .data$improved)
}
