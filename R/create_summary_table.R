#' Create Clinical Significance Summary Table
#'
#' @param data "categories" element from a clinisig object
#' @param n_obs "n_obs" element from a clinisig object, used for
#'   calculating percentages
#'
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer everything
#' @importFrom dplyr summarise mutate across
#' @importFrom tools toTitleCase
#'
#' @noRd
.create_summary_table <- function(data, n_obs) {
  # Check if a grouping variables was specified and group results if so
  if (.has_group(data)) {
    group_var <- as.symbol("group")
  } else {
    group_var <- NULL
  }

  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      across(.data$recovered:.data$harmed, sum)
    ) %>%
    pivot_longer(
      cols = .data$recovered:.data$harmed,
      names_to = "category",
      values_to = "n"
    ) %>%
    mutate(
      percent = .data$n / n_obs,
      category = toTitleCase(.data$category)
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
#' @importFrom tools toTitleCase
#'
#' @return A tibble with columns `category` and `percent`
#'
#' @noRd
.create_summary_table_ha <- function(data, r_dd, se_measurement, cutoff, sd_post) {
  reliability_post <- .calc_reliability_ha(sd_post, se_measurement)

  if (.has_group(data)) {
    group_var <- as.symbol("group")
  } else {
    group_var <- NULL
  }

  test_data %>%
    group_by({{ group_var }}) %>%
    summarise(
      mean_change = mean(change),
      sd_change = sd(change),
      m_post = mean(post),
      sd_post = sd(post)
    ) %>%
    mutate(
      z_changed = (0 - mean_change) / (sd_change * sqrt(r_dd)),
      changed = pnorm(z_changed),
      z_functional = (cutoff - m_post) / (sd_post * sqrt(reliability_post)),
      functional = pnorm(z_functional)
    ) %>%
    select(-matches(".*_.*")) %>%
    pivot_longer(
      cols = c(changed, functional),
      names_to = "category",
      values_to = "percent"
    ) %>%
    mutate(
      category = toTitleCase(category)
    )
}
