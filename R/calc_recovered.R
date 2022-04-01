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
      recovered = .data$clinical_pre & .data$functional_post & .data$improved
    ) %>%
    relocate(.data$clinical_pre, .data$functional_post, .data$recovered, .before = .data$improved) %>%
    relocate(.data$unchanged, .after = .data$improved)
}



#' Title
#'
#' @param cutoff_data
#' @param rci_data
#'
#' @return
#' @export
#'
#' @examples
.calc_recovered_ha <- function(cutoff_data, rci_data) {
  cutoff_data %>%
    left_join(rci_data, by = "id") %>%
    mutate(
      recovered = functional_post & improved,
      improved = ifelse(recovered, FALSE, improved)
    ) %>%
    relocate(rci, .after = cs_indiv) %>%
    relocate(recovered, .after = functional_post) %>%
    relocate(unchanged, .after = improved)
}
