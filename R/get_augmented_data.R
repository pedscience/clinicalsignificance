#' Get Data Augmented With Clinical Significance Categories
#'
#' @inheritParams get_data
#'
#' @importFrom dplyr left_join case_when
#' @importFrom rlang .data
#' @importFrom checkmate
#'
#' @return A tibble with used data and clinical significance categories
#' @export
get_augmented_data <- function(x) {
  assert_class(x, "clinisig")

  categories <- x[["categories"]]

  categories %>%
    mutate(
      category = case_when(
        recovered ~ "Recovered",
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated",
        harmed ~ "Harmed"
      ),
      category = factor(.data$category, levels = c("Recovered", "Improved", "Unchanged", "Deteriorated", "Harmed"))
    )
}
