#' Calculate Categories based on an RCI
#'
#' @param cutoff_data A data frame containing categorizations based on the
#'   cutoff, needs at least columns `clinical_pre` and `functional_post` which
#'   are logicals
#' @param rci_data A data frame containing categorizations based on the RCI,
#'   needs at least columns `ìmproved`, `unchanged`, and `deteriorated`
#'
#' @importFrom rlang .data
#' @importFrom dplyr relocate left_join
#'
#' @return The full data frame with categories
#'
#' @noRd
.calc_recovered <- function(data, cutoff_data, rci_data) {
  data %>%
    left_join(rci_data, by = "id") %>%
    left_join(cutoff_data, by = "id") %>%
    mutate(
      recovered = .data$clinical_pre & .data$functional_post & .data$improved,
      improved = ifelse(.data$recovered, FALSE, .data$improved),
      harmed = !.data$clinical_pre & !.data$functional_post & .data$deteriorated,
      deteriorated = ifelse(.data$harmed, FALSE, .data$deteriorated)
    ) %>%
    relocate(.data$clinical_pre, .data$functional_post, .data$recovered, .before = .data$improved) %>%
    relocate(.data$unchanged, .after = .data$improved)
}



#' Calculate Categories based on an RCI for Hagemann and Arrindell
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
.calc_recovered_ha <- function(data, cutoff_data, rci_data) {
  data %>%
    left_join(cutoff_data, by = "id") %>%
    left_join(rci_data, by = "id") %>%
    mutate(
      recovered = .data$functional_post & .data$improved,
      improved = ifelse(.data$recovered, FALSE, .data$improved),
      harmed = FALSE
    ) %>%
    relocate(.data$rci, .after = .data$cs_indiv) %>%
    relocate(.data$recovered, .after = .data$functional_post) %>%
    relocate(.data$unchanged, .after = .data$improved)
}
