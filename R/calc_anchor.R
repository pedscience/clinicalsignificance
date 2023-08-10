#' Generic to Calculate Anchor-Based Results
#'
#' @param data A datasets object
#' @param mid_improvement Numeric, change that indicates a clinically
#'   significant improvement
#' @param mid_deterioration Numeric, change that indicates a clinically
#'   significant deterioration
#' @param direction Which direction is beneficial? Lower = -1, better = 1
#'
#' @return An object of class `cs_anchor`
#' @export
calc_anchor <- function(data,
                        mid_improvement,
                        mid_deterioration,
                        direction,
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
calc_anchor.cs_anchor_individual_within <- function(data, mid_improvement, mid_deterioration, direction) {
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
calc_anchor.cs_anchor_group_within <- function(data, mid_improvement, mid_deterioration, direction, ci_level) {
  used_data <- data[["data"]]
  threshold <- direction * mid_improvement

  if (.has_group(used_data)) {
    results_tbl <- used_data |>
      tidyr::nest(.by = group) |>
      dplyr::mutate(
        results = purrr::map(data, \(x) tidy_t_test(x, ci_level = 0.95)),
        .keep = "unused"
      ) |>
      tidyr::unnest(results)
  } else {
    results_tbl <- tidy_t_test(used_data, ci_level = ci_level)
  }

  if (direction == -1) {
    out <- results_tbl |>
      dplyr::mutate(
        category = dplyr::case_when(
          sign(lower) != sign(upper) ~ "Statistically not significant",
          (sign(lower) == sign(upper)) & lower > threshold ~ "Statistically significant but not clincally relevant",
          upper > threshold & mean_difference > threshold & lower < threshold ~ "Not significantly less than the threshold",
          upper > threshold & mean_difference < threshold & lower < threshold ~ "Probably clinically significant effect",
          upper < threshold ~ "Large clinically significant effect"
        )
      )
  } else {
    out <- results_tbl |>
      dplyr::mutate(
        category = dplyr::case_when(
          (sign(lower) == sign(upper)) & upper < threshold ~ "Statistically significant but not clincally relevant",
          lower < threshold & mean_difference < threshold & upper > threshold ~ "Not significantly greater than the threshold",
          lower < threshold & mean_difference > threshold & upper > threshold ~ "Probably clinically significant effect",
          lower > threshold ~ "Large clinically significant effect"
        )
      )
  }
}


tidy_t_test <- function(data, ci_level) {
  results <- stats::t.test(data[["post"]], data[["pre"]], paired = TRUE, conf.level = ci_level)

  dplyr::tibble(
    mean_difference = results$estimate,
    lower = results$conf.int[[1]],
    upper = results$conf.int[[2]],
    ci = ci_level,
    n = nrow(data)
  )
}
