#' Calculate Categories based on an RCI
#'
#' @param cutoff_data A data frame containing categorizations based on the
#'   cutoff, needs at least columns `clinical_pre` and `functional_post` which
#'   are logicals
#' @param rci_data A data frame containing categorizations based on the RCI,
#'   needs at least columns `ìmproved`, `unchanged`, and `deteriorated`
#'
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
      recovered = clinical_pre & functional_post & improved,
      improved = ifelse(recovered, FALSE, improved),
      harmed = !clinical_pre & !functional_post & deteriorated,
      deteriorated = ifelse(harmed, FALSE, deteriorated)
    ) %>%
    relocate(clinical_pre, functional_post, recovered, .before = improved) %>%
    relocate(unchanged, .after = improved)
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
      recovered = functional_post & improved,
      improved = ifelse(recovered, FALSE, improved),
      harmed = FALSE
    ) %>%
    relocate(rci, .after = cs_indiv) %>%
    relocate(recovered, .after = functional_post) %>%
    relocate(unchanged, .after = improved)
}
