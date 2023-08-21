#' Generic to Calculate Anchor-Based Results
#'
#' @param data A datasets object
#' @param mid_improvement Numeric, change that indicates a clinically
#'   significant improvement
#' @param mid_deterioration Numeric, change that indicates a clinically
#'   significant deterioration
#' @param direction Which direction is beneficial? Lower = -1, better = 1
#' @param ci_level Numeric, desired confidence interval.
#'
#' @return An object of class `cs_anchor`
#' @export
calc_anchor <- function(data,
                        mid_improvement,
                        mid_deterioration,
                        reference_group,
                        post,
                        direction,
                        bayesian,
                        prior_scale,
                        ci_level) {
  UseMethod("calc_anchor")
}



#' Anchor Calculations for Individual Results
#'
#' This is an internal function and should never be called directly.
#'
#' @inheritParams calc_anchor
#'
#' @return An object of class `cs_anchor_individual_within`
#' @export
calc_anchor.cs_anchor_individual_within <- function(data, mid_improvement, mid_deterioration, direction, ci_level, ...) {
  out <- data[["data"]] |>
    dplyr::mutate(
      improved     = direction * change >= mid_improvement,
      deteriorated = direction * change <= -mid_deterioration,
      unchanged = !improved & !deteriorated
    ) |>
    dplyr::select(id, improved:unchanged)

  class(out) <- c("cs_anchor_individual_within", class(out))
  out
}



#' Anchor Calculations for Group Effect Within
#'
#' This is an internal function and should never be called directly.
#'
#' @inheritParams calc_anchor
#'
#' @return An object of class `cs_anchor_group_within`
#' @export
calc_anchor.cs_anchor_group_within <- function(data, mid_improvement, mid_deterioration, direction, ci_level, bayesian, prior_scale,...) {
  used_data <- data[["data"]]
  threshold <- direction * mid_improvement

  if (.has_group(used_data)) {
    results_tbl <- used_data |>
      tidyr::nest(.by = group) |>
      dplyr::mutate(
        results = purrr::map(data, \(x) t_test_within(x, ci_level = ci_level, bayesian = bayesian, prior_scale = prior_scale)),
        .keep = "unused"
      ) |>
      tidyr::unnest(results)
  } else {
    results_tbl <- t_test_within(used_data, ci_level = ci_level, bayesian = bayesian, prior_scale = prior_scale)
  }

  if (direction == -1) {
    out <- results_tbl |>
      dplyr::mutate(
        category = dplyr::case_when(
          sign(lower) != sign(upper) ~ "Statistically not significant",
          (sign(lower) == sign(upper)) & lower > threshold ~ "Statistically significant but not clinically relevant",
          upper > threshold & difference > threshold & lower < threshold ~ "Not significantly less than the threshold",
          upper > threshold & difference < threshold & lower < threshold ~ "Probably clinically significant effect",
          upper < threshold ~ "Large clinically significant effect"
        )
      )
  } else {
    out <- results_tbl |>
      dplyr::mutate(
        category = dplyr::case_when(
          (sign(lower) == sign(upper)) & upper < threshold ~ "Statistically significant but not clinically relevant",
          lower < threshold & difference < threshold & upper > threshold ~ "Not significantly greater than the threshold",
          lower < threshold & difference > threshold & upper > threshold ~ "Probably clinically significant effect",
          lower > threshold ~ "Large clinically significant effect"
        )
      )
  }
}




#' Anchor Calculations for Group Effect Between
#'
#' This is an internal function and should never be called directly.
#'
#' @inheritParams calc_anchor
#'
#' @return An object of class `cs_anchor_group_between`
#' @export
calc_anchor.cs_anchor_group_between <- function(data, mid_improvement, mid_deterioration, reference_group, post, direction, ci_level, bayesian, prior_scale) {
  threshold <- direction * mid_improvement
  if (is.null(reference_group)) {
    if (is.factor(data[["group"]])) {
      reference_group <- data |>
        dplyr::distinct(group) |>
        dplyr::arrange(group) |>
        dplyr::slice_head(n = 1) |>
        dplyr::pull(group) |>
        as.character()
    } else {
      reference_group <- data |>
        dplyr::distinct(group) |>
        dplyr::arrange(group) |>
        dplyr::slice_head(n = 1) |>
        dplyr::pull(group)
    }
  }


  # Get factor levels for comparison groups
  if (is.factor(data[["group"]])) {
    factor_levels <- dplyr::distinct(data, group) |>
      dplyr::filter(group != reference_group) |>
      dplyr::pull(group) |>
      droplevels() |>
      levels()

    comparison_groups <- factor_levels |>
      as.character()
  } else {
    comparison_groups <- dplyr::distinct(data, group) |>
      dplyr::filter(group != reference_group) |>
      dplyr::pull(group)
  }


  # Calculate results for all pairwise comparisons
  results_tbl <- tidyr::crossing(
    reference = reference_group,
    comparison = comparison_groups,
    data = list(data)
  ) |>
    dplyr::mutate(
      data = purrr::pmap(list(data, reference, comparison), \(a, b, c) dplyr::filter(a, group %in% c(b, c), time == post)),
      results = purrr::map2(data, reference, \(a, b) t_test_between(data = a, reference_group = b, ci_level = ci_level, bayesian = bayesian, prior_scale = prior_scale))
    ) |>
    dplyr::select(-data) |>
    tidyr::unnest(results)


  # Determine categories
  if (direction == -1) {
    out <- results_tbl |>
      dplyr::mutate(
        category = dplyr::case_when(
          sign(lower) != sign(upper) ~ "Statistically not significant",
          (sign(lower) == sign(upper)) & lower > threshold ~ "Statistically significant but not clinically relevant",
          upper > threshold & difference > threshold & lower < threshold ~ "Not significantly less than the threshold",
          upper > threshold & difference < threshold & lower < threshold ~ "Probably clinically significant effect",
          upper < threshold ~ "Large clinically significant effect"
        )
      )
  } else {
    out <- results_tbl |>
      dplyr::mutate(
        category = dplyr::case_when(
          (sign(lower) == sign(upper)) & upper < threshold ~ "Statistically significant but not clinically relevant",
          lower < threshold & difference < threshold & upper > threshold ~ "Not significantly greater than the threshold",
          lower < threshold & difference > threshold & upper > threshold ~ "Probably clinically significant effect",
          lower > threshold ~ "Large clinically significant effect"
        )
      )
  }

  if (is.factor(data[["group"]])) {
    out <- out |>
      dplyr::mutate(comparison = factor(comparison, levels = factor_levels)) |>
      dplyr::arrange(comparison)
  }

  out
}




t_test_within <- function(data, group_1 = "post", group_2 = "pre", ci_level = 0.95, bayesian = FALSE, prior_scale = sqrt(2)/2) {
  if (!bayesian) {
    results <- stats::t.test(data[[group_1]], data[[group_2]], paired = TRUE, conf.level = ci_level)

    dplyr::tibble(
      difference = results$estimate,
      lower = results$conf.int[[1]],
      upper = results$conf.int[[2]],
      ci = ci_level,
      n = nrow(data)
    )
  } else {
    BayesFactor::ttestBF(data[[group_1]], data[[group_2]], paired = TRUE, rscale = prior_scale) |>
      bayestestR::describe_posterior(ci = ci_level) |>
      dplyr::as_tibble() |>
      dplyr::select(
        difference = Median,
        lower = CI_low,
        upper = CI_high,
        ci = CI
      ) |>
      dplyr::mutate(n = nrow(data))
  }
}



t_test_between <- function(data, reference_group, ci_level, bayesian = FALSE, prior_scale = sqrt(2)/2) {
  # Get data vectors
  reference_data <- data |>
    dplyr::filter(group == reference_group) |>
    dplyr::pull(outcome) |>
    na.omit()

  comparison_data <- data |>
    dplyr::filter(group != reference_group) |>
    dplyr::pull(outcome) |>
    na.omit()


  if (!bayesian) {
    # t test results
    results <- t.test(comparison_data, reference_data, data = data, conf.level = ci_level)


    # Output a tibble
    dplyr::tibble(
      difference = results$estimate[[1]] - results$estimate[[2]],
      lower = results$conf.int[[1]],
      upper = results$conf.int[[2]],
      ci = ci_level,
      n_reference = length(reference_data),
      n_comparison = length(comparison_data)
    )
  } else {
    BayesFactor::ttestBF(comparison_data, reference_data, rscale = prior_scale) |>
      bayestestR::describe_posterior() |>
      dplyr::select(
        difference = Median,
        lower = CI_low,
        upper = CI_high,
        ci = CI
      ) |>
      dplyr::mutate(
        n_reference = length(reference_data),
        n_comparison = length(comparison_data)
      )
  }
}
