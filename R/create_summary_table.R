#' Generic For Creating Summary Tables
#'
#' This is an internal function used to create summmary table for the different
#' clinical significance methods. This should not be used directly.
#'
#' @param rci_results RCI results object
#' @param cutoff_results Cutoff results object
#' @param data A dataframe for joining the result with. Must contain column
#'   `id`.
#' @param method Clinical significance method
#' @param ... Additional arguments
#'
#' @return A tibble
#' @export
#'
#' @noRd
create_summary_table <- function(rci_results,
                                 cutoff_results,
                                 data,
                                 method,
                                 r_dd,
                                 se_measurement,
                                 cutoff,
                                 sd_post,
                                 direction,
                                 ...) {
  UseMethod("create_summary_table")
}



#' Create Summary Table for Distribution-Based Approach
#'
#' @param x RCI results
#' @param data The used dataframe
#' @param ... Additional arguments
#'
#' @return A tibble containing the category, respective n, and percent
#' @export
#'
#' @noRd
create_summary_table.cs_distribution <- function(rci_results, data, ...) {
  # Get the RCI results as well as the used data (needed if grouped results are
  # required)
  rci_results <- rci_results[["data"]]
  used_data <- data[["data"]]


  # Check if data has a group column
  if (.has_group(used_data)) group_var <- as.symbol("group") else group_var <- NULL


  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  joined_data <- used_data |>
    dplyr::left_join(rci_results, dplyr::join_by("id"))


  # Count all cases per category and calculate relative amount (percentages)
  summary <- joined_data |>
    dplyr::summarise(
      dplyr::across(improved:unchanged, sum), .by = tidyr::all_of(group_var)
    ) |>
    tidyr::pivot_longer(
      cols = improved:unchanged,
      names_to = "category",
      values_to = "n"
    ) |>
    dplyr::mutate(
      percent = n / sum(n),
      category = tools::toTitleCase(category),
      category = factor(category, levels = c("Improved", "Unchanged", "Deteriorated"))
    )

  if (!.has_group(used_data)) dplyr::arrange(summary, category) else dplyr::arrange(summary, group, category)
}




#' Create Summary Table for Statistical Approach
#'
#' @inheritParams create_summary_table
#'
#' @return A tibble containing the category, respective n, and percent
#' @export
#'
#' @noRd
create_summary_table.cs_statistical <- function(cutoff_results, data, method, ...) {
  # Get the cutoff results as well as the used data (needed if grouped results
  # are required)
  cutoff_results <- cutoff_results[["data"]]
  used_data <- data[["data"]]


  # Check if data has a group column
  if (.has_group(used_data)) group_var <- as.symbol("group") else group_var <- NULL


  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  joined_data <- used_data |>
    dplyr::left_join(cutoff_results, dplyr::join_by("id"))


  # Count all cases per category and calculate relative amount (percentages)
  if (method != "HA") {
    categories <- joined_data |>
      dplyr::mutate(
        improved = ifelse(clinical_pre & functional_post, TRUE, FALSE),
        deteriorated = ifelse(!clinical_pre & !functional_post, TRUE, FALSE),
        unchanged = ifelse(!improved & !deteriorated, TRUE, FALSE)
      )
  } else {
    categories <- joined_data |>
      dplyr::rename(improved = functional_post) |>
      dplyr::mutate(unchanged = ifelse(!improved, TRUE, FALSE))
  }


  # Create summary table
  summary <- categories |>
    dplyr::summarise(
      dplyr::across(improved:unchanged, sum), .by = tidyr::all_of(group_var)
    ) |>
    tidyr::pivot_longer(
      cols = improved:unchanged,
      names_to = "category",
      values_to = "n"
    ) |>
    dplyr::mutate(
      percent = n / sum(n),
      category = tools::toTitleCase(category),
      category = factor(category, levels = c("Improved", "Unchanged", "Deteriorated"))
    )

  if (!.has_group(used_data)) dplyr::arrange(summary, category) else dplyr::arrange(summary, group, category)
}




#' Create Summary Table for Combined Approach
#'
#' @inheritParams create_summary_table
#'
#' @return A tibble containing the category, respective n, and percent
#' @export
#'
#' @noRd
create_summary_table.cs_combined <- function(rci_results, cutoff_results, data, method, r_dd, se_measurement, cutoff, sd_post, direction, ...) {
  # Get all results as well as the used data (needed if grouped results are
  # required)
  cutoff_results <- cutoff_results[["data"]]
  rci_results <- rci_results[["data"]]
  used_data <- data[["data"]]


  # Check if data has a group column
  if (.has_group(used_data)) group_var <- as.symbol("group") else group_var <- NULL


  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  joined_data <- used_data |>
    dplyr::left_join(cutoff_results, dplyr::join_by("id")) |>
    dplyr::left_join(rci_results, dplyr::join_by("id"))


  # Count all cases per category and calculate relative amount (percentages)
  if (method != "HA") {
    categories <- joined_data |>
      dplyr::mutate(
        recovered = clinical_pre & functional_post & improved,
        improved = ifelse(recovered, FALSE, improved),
        harmed = !clinical_pre & !functional_post & deteriorated,
        deteriorated = ifelse(harmed, FALSE, deteriorated)
      ) |>
      dplyr::relocate(clinical_pre, functional_post, recovered, .before = improved) |>
      dplyr::relocate(unchanged, .after = improved) |>
      dplyr::select(-c(clinical_pre, functional_post))
  } else {
    categories <- joined_data |>
      dplyr::mutate(
        recovered = functional_post & improved,
        improved = ifelse(recovered, FALSE, improved),
        harmed = FALSE
      ) |>
      dplyr::relocate(rci, .after = cs_indiv) |>
      dplyr::relocate(recovered, .after = functional_post) |>
      dplyr::relocate(unchanged, .after = improved)
  }


  # Create summary table
  summary <- categories |>
    dplyr::summarise(
      dplyr::across(recovered:harmed, sum), .by = tidyr::all_of(group_var)
    ) |>
    tidyr::pivot_longer(
      cols = recovered:harmed,
      names_to = "category",
      values_to = "n"
    ) |>
    dplyr::mutate(
      percent = n / sum(n),
      category = tools::toTitleCase(category),
      category = factor(category, levels = c("Recovered", "Improved", "Unchanged", "Deteriorated", "Harmed"))
    )

  if (!.has_group(used_data)) individual_level_summary <- dplyr::arrange(summary, category) else individual_level_summary <- dplyr::arrange(summary, group, category)
  group_level_summary <- NA


  # Create group level summary table for HA method
  if (method == "HA") {
    reliability_post <- .calc_reliability_ha(sd_post, se_measurement)

    summary_statistics <- used_data |>
      dplyr::summarise(
        mean_change = mean(change),
        sd_change = sd(change),
        m_post = mean(post),
        sd_post = sd(post),
        .by = tidyr::all_of(group_var)
      )

    if (direction == 1) {
      group_level_pcts <- summary_statistics |>
        dplyr::mutate(
          z_changed = (0 - mean_change) / (sd_change * sqrt(r_dd)),
          changed = 1 - pnorm(z_changed),
          z_functional = (cutoff - m_post) / (sd_post * sqrt(reliability_post)),
          functional = 1 - pnorm(z_functional)
        )
    } else if (direction == -1) {
      group_level_pcts <- summary_statistics |>
        dplyr::mutate(
          z_changed = (0 - mean_change) / (sd_change * sqrt(r_dd)),
          changed = pnorm(z_changed),
          z_functional = (cutoff - m_post) / (sd_post * sqrt(reliability_post)),
          functional = pnorm(z_functional)
        )
    }

    group_level_summary <- group_level_pcts |>
      dplyr::select(-matches(".*_.*")) |>
      tidyr::pivot_longer(
        cols = c(changed, functional),
        names_to = "category",
        values_to = "percent"
      ) |>
      dplyr::mutate(
        category = tools::toTitleCase(category)
      )
  }

  list(
    individual_level_summary = individual_level_summary,
    group_level_summary = group_level_summary,
    categories = categories
  )
}


#' Create Summary Table for Percentage-Change Approach
#'
#' @param x RCI results
#' @param data The used dataframe
#' @param ... Additional arguments
#'
#' @return A tibble containing the category, respective n, and percent
#' @export
#'
#' @noRd
create_summary_table.cs_percentage <- function(pct_results, data, ...) {
  # Get the percentage results as well as the used data (needed if grouped
  # results are required)
  used_data <- data[["data"]]


  # Check if data has a group column
  if (.has_group(used_data)) group_var <- as.symbol("group") else group_var <- NULL


  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  joined_data <- used_data |>
    dplyr::left_join(pct_results, dplyr::join_by("id"))


  # Count all cases per category and calculate relative amount (percentages)
  summary <- joined_data |>
    dplyr::summarise(
      dplyr::across(improved:unchanged, sum), .by = tidyr::all_of(group_var)
    ) |>
    tidyr::pivot_longer(
      cols = improved:unchanged,
      names_to = "category",
      values_to = "n"
    ) |>
    dplyr::mutate(
      percent = n / sum(n),
      category = tools::toTitleCase(category),
      category = factor(category, levels = c("Improved", "Unchanged", "Deteriorated"))
    )

  if (!.has_group(used_data)) dplyr::arrange(summary, category) else dplyr::arrange(summary, group, category)
}


#' #' Create Clinical Significance Summary Table
#' #'
#' #' @param data "categories" element from a clinisig object
#' #' @param n_obs "n_obs" element from a clinisig object, used for
#' #'   calculating percentages
#' #'
#' #' @importFrom tidyr pivot_longer everything
#' #' @importFrom dplyr summarise mutate across group_by ungroup
#' #' @importFrom tools toTitleCase
#' #'
#' #' @noRd
#' .create_summary_table <- function(data) {
#'   # Check if a grouping variables was specified and group results if so
#'   if (.has_group(data)) {
#'     group_var <- as.symbol("group")
#'   } else {
#'     group_var <- NULL
#'   }
#'
#'   data |>
#'     group_by({{ group_var }}) |>
#'     summarise(
#'       across(recovered:harmed, sum),
#'       .groups = "drop"
#'     ) |>
#'     pivot_longer(
#'       cols = recovered:harmed,
#'       names_to = "category",
#'       values_to = "n"
#'     ) |>
#'     group_by({{ group_var }}) |>
#'     mutate(
#'       percent = n / sum(n),
#'       category = toTitleCase(category)
#'     ) |>
#'     ungroup()
#' }
#'
#' #' Create group level summary table like Hagemann and Arrindell
#' #'
#' #' @param data The used data frame
#' #' @param r_dd Reliability of differences scores
#' #' @param se_measurement Standard error of measurement
#' #' @param cutoff True cutoff score
#' #' @param mean_post Mean of post meausurement
#' #' @param sd_post SD of post measurement
#' #' @param direction Which direction is better? 1 = higher, -1 = lower
#' #'
#' #' @importFrom stats pnorm sd
#' #' @importFrom dplyr tibble matches
#' #' @importFrom tools toTitleCase
#' #'
#' #' @return A tibble with columns `category` and `percent`
#' #'
#' #' @noRd
# .create_summary_table_ha <- function(data, r_dd, se_measurement, cutoff, sd_post, direction) {
#   reliability_post <- .calc_reliability_ha(sd_post, se_measurement)
#
#   if (.has_group(data)) {
#     group_var <- as.symbol("group")
#   } else {
#     group_var <- NULL
#   }
#
#   summaries <- data |>
#     group_by({{ group_var }}) |>
#     summarise(
#       mean_change = mean(change),
#       sd_change = sd(change),
#       m_post = mean(post),
#       sd_post = sd(post)
#     )
#
#   if (direction == 1) {
#     group_level_pcts <- summaries |>
#       mutate(
#         z_changed = (0 - mean_change) / (sd_change * sqrt(r_dd)),
#         changed = 1 - pnorm(z_changed),
#         z_functional = (cutoff - m_post) / (sd_post * sqrt(reliability_post)),
#         functional = 1 - pnorm(z_functional)
#       )
#   } else if (direction == -1) {
#     group_level_pcts <- summaries |>
#       mutate(
#         z_changed = (0 - mean_change) / (sd_change * sqrt(r_dd)),
#         changed = pnorm(z_changed),
#         z_functional = (cutoff - m_post) / (sd_post * sqrt(reliability_post)),
#         functional = pnorm(z_functional)
#       )
#   }
#
#   group_level_pcts |>
#     select(-matches(".*_.*")) |>
#     pivot_longer(
#       cols = c(changed, functional),
#       names_to = "category",
#       values_to = "percent"
#     ) |>
#     mutate(
#       category = toTitleCase(category)
#     )
# }
