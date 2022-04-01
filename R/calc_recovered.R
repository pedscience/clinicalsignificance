#' Calculate Categories based on an RCI
#'
#' @param cutoff_data A data frame containing categorizations based on the
#'   cutoff
#' @param rci_data A data frame containing categorizations based on the RCI
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
      recovered = .data$clinical_pre & .data$functional_post & .data$improved,
      improved = ifelse(.data$recovered, FALSE, .data$improved)
    ) %>%
    relocate(.data$clinical_pre, .data$functional_post, .data$recovered, .before = .data$improved) %>%
    relocate(.data$unchanged, .after = .data$improved)
}



#' Calculate Categories based on an RCI
#'
#' @param cutoff_data A data frame containing categorizations based on the
#'   cutoff
#' @param rci_data A data frame containing categorizations based on the RCI
#'
#' @importFrom rlang .data
#'
#' @return The full data frame with categories
#'
#' @noRd
.calc_recovered_ha <- function(cutoff_data, rci_data) {
  cutoff_data %>%
    left_join(rci_data, by = "id") %>%
    mutate(
      recovered = .data$functional_post & .data$improved,
      improved = ifelse(.data$recovered, FALSE, .data$improved)
    ) %>%
    relocate(.data$rci, .after = .data$cs_indiv) %>%
    relocate(.data$recovered, .after = .data$functional_post) %>%
    relocate(.data$unchanged, .after = .data$improved)
}
