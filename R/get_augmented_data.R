#' Get Data Augmented With Clinical Significance Categories
#'
#' @inheritParams get_data
#'
#' @importFrom dplyr left_join case_when relocate
#' @importFrom rlang .data
#'
#' @return A tibble with used data and clinical significance categories
#' @export
get_augmented_data <- function(x) {
  assert_class(x, "clinisig")

  clinisig_method <- get_clinical_significance_method(x)

  if (clinisig_method == "HLM") {
    hlm_categories <- x[["categories"]]
    hlm_coefficients <- x[["rci"]][["coefficients"]] %>%
      select(id, intercept, slope, eb_slope)

    categories <- hlm_categories %>%
      left_join(hlm_coefficients, by = "id") %>%
      relocate(intercept:eb_slope, .after = post)
  } else {
    categories <- x[["categories"]]
  }

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
