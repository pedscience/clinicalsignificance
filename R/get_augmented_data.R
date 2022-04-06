#' Get Data Augmented With Clinical Significance Categories
#'
#' @inheritParams get_data
#'
#' @importFrom dplyr left_join case_when
#'
#' @return A tibble with used data and clinical significance categories
#' @export
get_augmented_data <- function(x) {
  used_data <- x[["datasets"]][["data"]]
  categories <- x[["categories"]]

  left_join(used_data, categories, by = "id") %>%
    mutate(
      category = case_when(
        recovered ~ "Recovered",
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated",
        harmed ~ "Harmed"
      ),
      category = factor(category, levels = c("Recovered", "Improved", "Unchanged", "Deteriorated", "Harmed"))
    )
}
