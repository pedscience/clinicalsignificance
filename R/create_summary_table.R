#' Create Clinical Significance Summary Table
#'
#' @param data "categories" element from a clinisig object
#' @param n_obs "n_obs" element from a clinisig object, used for
#'   calculating percentages
#'
#' @importFrom tidyr pivot_longer everything
#' @importFrom dplyr summarise mutate across
#'
#' @noRd
.create_summary_table <- function(data, n_obs) {
  recovered <- deteriorated <- n <-  NULL

  data %>%
    summarise(
      across(recovered:deteriorated, sum)
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "category",
      values_to = "n"
    ) %>%
    mutate(
      percent = n / n_obs
    )
}
