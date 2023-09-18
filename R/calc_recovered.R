#' Calculate Categories based on an RCI
#'
#' @param cutoff_data A data frame containing categorizations based on the
#'   cutoff, needs at least columns `clinical_pre` and `functional_post` which
#'   are logicals
#' @param rci_data A data frame containing categorizations based on the RCI,
#'   needs at least columns `ìmproved`, `unchanged`, and `deteriorated`
#'
#'
#' @return The full data frame with categories
#'
#' @noRd
.calc_recovered <- function(data, cutoff_data, rci_data) {
  data |>
    dplyr::left_join(rci_data, by = "id") |>
    dplyr::left_join(cutoff_data, by = "id") |>
    dplyr::mutate(
      recovered = clinical_pre & functional_post & improved,
      improved = ifelse(recovered, FALSE, improved),
      harmed = !clinical_pre & !functional_post & deteriorated,
      deteriorated = ifelse(harmed, FALSE, deteriorated)
    ) |>
    dplyr::relocate(clinical_pre, functional_post, recovered, .before = improved) |>
    dplyr::relocate(unchanged, .after = improved)
}



#' Calculate Categories based on an RCI for Hagemann and Arrindell
#'
#' @param cutoff_data A data frame containing categorizations based on the
#'   cutoff
#' @param rci_data A data frame containing categorizations based on the RCI
#'
#' @return The full data frame with categories
#'
#' @noRd
.calc_recovered_ha <- function(data, cutoff_data, rci_data) {
  data |>
    dplyr::left_join(cutoff_data, by = "id") |>
    dplyr::left_join(rci_data, by = "id") |>
    dplyr::mutate(
      recovered = functional_post & improved,
      improved = ifelse(recovered, FALSE, improved),
      harmed = FALSE
    ) |>
    dplyr::relocate(rci, .after = cs_indiv) |>
    dplyr::relocate(recovered, .after = functional_post) |>
    dplyr::relocate(unchanged, .after = improved)
}
