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
#' @importFrom dplyr relocate
#'
#' @return The full data frame with categories
#'
#' @noRd
.calc_categories <- function(cutoff_data, rci_data) {
  rci_data %>%
    left_join(cutoff_data, by = "id") %>%
    mutate(
      recovered = clinical_pre & functional_post & improved
    ) %>%
    relocate(clinical_pre, functional_post, recovered, .before = improved) %>%
    relocate(unchanged, .after = improved)
}
