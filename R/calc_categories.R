#' Calculate Categories based on an RCI
#'
#' @param data A preprocessed data frame
#' @param cutoff A list containing cutoff information
#' @param rci A vector containing RCIs
#' @param rci_cutoff A numveric value, the multiplier according to which a
#'   patient is considered reliably changed
#'
#' @inheritParams .calc_cutoff
#'
#' @importFrom rlang .data
#' @importFrom dplyr relocate
#'
#' @return The full data frame with categories
#'
#' @noRd
.calc_categories_jacobson <- function(data, cutoff, rci, rci_cutoff = 1.96, direction = 1) {
  clinical_cutoff <- cutoff$cutoff

  data  %>%
    bind_cols(rci = rci) %>%
    mutate(
      clinical_pre    = ifelse(direction * .data$pre < direction * clinical_cutoff, TRUE, FALSE),
      functional_post = ifelse(direction * .data$post > direction * clinical_cutoff, TRUE, FALSE),
      improved        = ifelse(direction * rci > rci_cutoff, TRUE, FALSE),
      deteriorated    = ifelse(direction * rci < -rci_cutoff, TRUE, FALSE),
      recovered       = .data$clinical_pre & .data$functional_post & .data$improved,
      unchanged       = !.data$improved & !.data$deteriorated
    ) %>%
    relocate(recovered, improved, unchanged, deteriorated, .after = functional_post)
}
