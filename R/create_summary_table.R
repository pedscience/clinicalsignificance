#' Create Clinical Significance Summary Table
#'
#' @param data "categories" element from a clinisig object
#' @param n_obs "n_obs" element from a clinisig object, used for
#'   calculating percentages
#'
#' @importFrom tidyr pivot_longer everything
#' @importFrom dplyr summarise mutate across
#' @importFrom tools toTitleCase
#'
#' @noRd
.create_summary_table <- function(data, n_obs) {
  recovered <- deteriorated <- n <-  NULL

  data %>%
    summarise(
      across(recovered:harmed, sum)
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "category",
      values_to = "n"
    ) %>%
    mutate(
      percent = n / n_obs,
      category = toTitleCase(category)
    )
}

#' Create group level summary table like Hagemann and Arrindell
#'
#' @param data The used data frame
#' @param r_dd Reliability of differences scores
#' @param se_measurement Standard error of measurement
#' @param cutoff True cutoff score
#' @param mean_post Mean of post meausurement
#' @param sd_post SD of post measurement
#'
#' @importFrom stats pnorm sd
#' @importFrom dplyr tibble
#'
#' @return A tibble with columns `category` and `percent`
#'
#' @noRd
.create_summary_table_ha <- function(data, r_dd, se_measurement, cutoff, mean_post, sd_post) {
  # Proportion that changed
  mean_change <- mean(data$change)
  sd_change <- sd(data$change)

  z_changed <- (0 - mean_change) / (sd_change * sqrt(r_dd))
  prop_changed <- pnorm(z_changed)


  # Proportion that is below cutoff (i.e., functional)
  reliability_post <- .calc_reliability_ha(sd_post, se_measurement)

  z_functional <- (cutoff - mean_post) / (sd_post * sqrt(reliability_post))
  prop_functional <- pnorm(z_functional)


  # Join both values in a data frame
  tibble(
    category = c("changed", "functional"),
    percent = c(prop_changed, prop_functional)
  )
}
