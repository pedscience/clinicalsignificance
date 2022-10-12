#' Get Data Augmented With Clinical Significance Categories
#'
#' @inheritParams get_data
#'
#' @importFrom dplyr left_join case_when relocate
#' @importFrom rlang .data
#'
#' @return A tibble with used data and clinical significance categories
#' @export
#'
#' @examples
#' results <- jacobson_1989 %>%
#' clinical_significance(
#'   id = subject,
#'   time = time,
#'   outcome = gds,
#'   pre = "pre",
#'   reliability = 0.80,
#'   m_functional = 30,
#'   sd_functional = 7,
#'   type = "c"
#' )
#'
#' get_augmented_data(results)

get_augmented_data <- function(x) {
  assert_class(x, "clinisig")

  clinisig_method <- get_method(x)

  if (clinisig_method == "HLM") {
    hlm_categories <- x[["categories"]]
    hlm_coefficients <- x[["rci"]][["coefficients"]] %>%
      select(.data$id, .data$eb_estimate, .data$sd)

    categories <- hlm_categories %>%
      left_join(hlm_coefficients, by = "id") %>%
      relocate(.data$eb_estimate:.data$sd, .after = .data$post)
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
