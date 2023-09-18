#' Anchor-Based Analysis of Clinical Significance
#'
#' @description `cs_anchor()` can be used to determine the clinical significance
#'   of intervention studies employing the anchor-based approach. For this, a
#'   predefined minimally important difference (MID) for an instrument is known
#'   that corresponds to an important symptom improvement for patients. The data
#'   can then be analyzed on the individual as well as the group level to
#'   estimate, if the change because of an intervention is clinically
#'   significant.
#'
#' @section Computational details: For the individual-level analyses, the
#'   analysis is straight forward. An MID can be specified for an improvement as
#'   well as a deterioration (because these must not necessarily be identical)
#'   and the function basically counts how many patients fall within the MID
#'   range for both, improvement and deterioration, or how many patients exceed
#'   the limits of this range in either direction. A patient may than be
#'   categorized as:
#'   - Improved, the patient demonstrated a change that is equal or greater then
#'   the MID for an improvement
#'   - Unchanged, the patient demonstrated a change that is less than both MIDs
#'   - Deteriorated, the patient demonstrated a change that is equal or greater
#'   then the MID for a deterioration
#'
#'   For group-level analyses, the whole sample is either treated as a single
#'   group or is split up by grouping presented in the data. For within group
#'   analyses, the function calculates the median change from pre to post
#'   intervention with the associated credible interval (CI). Based on the
#'   median change and the limits of this CI, a group change can be categorized
#'   in 5 distinctive categories:
#'   - Statistically not significant, the CI contains 0
#'   - Statistically significant but not clinically relevant, the CI does not
#'   contain 0, but the median and both CI limits are beneath the MID threshold
#'   - Not significantly less than the threshold, the MID threshold falls within
#'   the CI but the median is still below that threshold
#'   - Probably clinically significant effect, the median crossed the MID
#'   threshold but the threshold is still inside the CI
#'   - Large clinically significant effect, the median crossed the MID threshold
#'   and the CI does not contain the threshold
#'
#'   If a between group comparison is desired, a reference group can be defined
#'   with the `reference_group` argument to which all subsequent groups are
#'   compared. This is usually an inactive comparator such as a placebo or
#'   wait-list control group. The difference between the pairwise compared
#'   groups is categorized just as the within group difference above, so the
#'   same categories apply.
#'
#'   The approach can be changed to a classical frequentist framework for which
#'   the point estimate then represents the mean difference and the CI a
#'   confidence interval. For an extensive overview over the differences between
#'   a Bayesian and frequentist CI, refer to Hespanhol et al. (2019).
#'
#' @inheritSection cs_distribution Data preparation
#'
#'
#' @inheritParams cs_distribution
#' @param mid_improvement Numeric, change that indicates a clinically
#'   significant improvement
#' @param mid_deterioration Numeric, change that indicates a clinically
#'   significant deterioration (optional). If `mid_deterioration` is not
#'   provided, it will be assumed to be equal to `mid_improvement`
#' @param target String, whether an individual or group analysis should be
#'   calculated. Available are
#'   - `"individual"` (the default) for which every individual participant is
#'   evaluated
#'   - `"group"` for which only the group wise effect is evaluated
#' @param effect String, if `target = "group"`, specify which effect should be
#'   calculated. Available are
#'   - `"within"` (the default), which yields the mean pre-post intervention
#'   difference with associated confidence intervals
#'   - `"between"`, which estimates the group wise mean difference and
#'   confidence intervals between two or more groups specified with the `group`
#'   argument at the specified measurement supplied with the `post`- argument
#'   The reference group may be supplied with `reference_group`
#' @param bayesian Logical, only relevant if `target = "group"`. Indicates if a
#'   Bayesian estimate (i.e., the median) of group differences with a credible
#'   interval should be calculated (if set to `TRUE`, the default) or a
#'   frequentist mean difference with confidence interval (if set to `FALSE`)
#' @param prior_scale String or numeric, can be adjusted to change the Bayesian
#'   prior distribution. See the documentation for `rscale` in
#'   [BayesFactor::ttestBF()] for details.
#' @param reference_group Specify the reference group to which all subsequent
#'   groups are compared against if `target = "group"` and `effect = "within"`
#'   (optional). Otherwise, the first distinct group is chosen based on
#'   alphabetical, numerical or factor ordering.
#' @param ci_level Numeric, define the credible or confidence interval level.
#'   The default is 0.95 for a 95%-CI.
#'
#' @references Hespanhol, L., Vallio, C. S., Costa, L. M., & Saragiotto, B. T.
#'   (2019). Understanding and interpreting confidence and credible intervals
#'   around effect estimates. Brazilian Journal of Physical Therapy, 23(4),
#'   290–301. https://doi.org/10.1016/j.bjpt.2018.12.006
#'
#' @family main
#'
#' @return An S3 object of class `cs_analysis` and `cs_anchor`
#' @export
#'
#' @examples
#' cs_results <- antidepressants |>
#'   cs_anchor(patient, measurement, mom_di, mid_improvement = 8)
#'
#' cs_results
#' plot(cs_results)
#'
#' # Set argument "pre" to avoid a warning
#' cs_results <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     pre = "Before",
#'     mid_improvement = 8
#'   )
#'
#'
#' # Inlcude the MID for deterioration
#' cs_results_with_deterioration <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     pre = "Before",
#'     mid_improvement = 8,
#'     mid_deterioration = 5
#'   )
#'
#' cs_results_with_deterioration
#' summary(cs_results_with_deterioration)
#' plot(cs_results_with_deterioration)
#'
#'
#' # Group the results by experimental condition
#' cs_results_grouped <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     pre = "Before",
#'     group = condition,
#'     mid_improvement = 8,
#'     mid_deterioration = 5
#'   )
#'
#' cs_results_grouped
#' summary(cs_results_grouped)
#' plot(cs_results_grouped)
#'
#' # The plot method always returns a ggplot2 object, so the plot may be further
#' # modified with ggplot2 code, e.g., facetting to avoid overplotting of groups
#' plot(cs_results_grouped) +
#'   ggplot2::facet_wrap(~ group)
#'
#'
#' # Compute group wise results
#' cs_results_groupwise <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     pre = "Before",
#'     mid_improvement = 8,
#'     target = "group"
#'   )
#'
#' cs_results_groupwise
#' summary(cs_results_groupwise)
#' plot(cs_results_groupwise)
#'
#'
#' # Group wise analysis, but split by experimentawl condition
#' cs_results_groupwise_condition <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     pre = "Before",
#'     group = condition,
#'     mid_improvement = 8,
#'     target = "group"
#'   )
#'
#' cs_results_groupwise_condition
#' summary(cs_results_groupwise_condition)
#' plot(cs_results_groupwise_condition)
#'
#'
#' # Compare all groups to a predefined reference group at a predefined measurement
#' cs_results_groupwise_between <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     post = "After",
#'     group = condition,
#'     mid_improvement = 8,
#'     target = "group",
#'     effect = "between"
#'   )
#'
#' cs_results_groupwise_between
#' summary(cs_results_groupwise_between)
#' plot(cs_results_groupwise_between)
#'
#'
#' # Compare all groups to a predefined reference group with frequentist appraoch
#' cs_results_groupwise_between_freq <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     post = "After",
#'     group = condition,
#'     mid_improvement = 8,
#'     target = "group",
#'     effect = "between",
#'     bayesian = FALSE
#'   )
#'
#' cs_results_groupwise_between_freq
#' summary(cs_results_groupwise_between_freq)
#' plot(cs_results_groupwise_between_freq)
cs_anchor <- function(data,
                      id,
                      time,
                      outcome,
                      group,
                      pre = NULL,
                      post = NULL,
                      mid_improvement = NULL,
                      mid_deterioration = NULL,
                      better_is = c("lower", "higher"),
                      target = c("individual", "group"),
                      effect = c("within", "between"),
                      bayesian = TRUE,
                      prior_scale = "medium",
                      reference_group = NULL,
                      ci_level = 0.95) {
  cs_target <- rlang::arg_match(target)
  cs_effect <- rlang::arg_match(effect)

  # Check arguments
  if (missing(id)) cli::cli_abort("Argument {.code id} is missing with no default. A column containing patient-specific IDs must be supplied.")
  if (missing(time)) cli::cli_abort("Argument {.code time} is missing with no default. A column identifying the individual measurements must be supplied.")
  if (missing(outcome)) cli::cli_abort("Argument {.code outcome} is missing with no default. A column containing the outcome must be supplied.")
  if (is.null(mid_improvement)) cli::cli_abort("Argument {.code mid_improvement} is missing with no default. A percentage change that indicates clinically signifcant change must be supplied.")
  if (!is.null(mid_improvement) & !is.numeric(mid_improvement)) cli::cli_abort("{.code mid_improvement} must be numeric but a {.code {typeof(mid_improvement)}} was supplied.")
  if (!is.null(mid_improvement) & mid_improvement < 0) cli::cli_abort("{.code mid_improvement} must be greater than 0 but {mid_improvement} was supplied.")
  if (!dplyr::between(ci_level, 0, 1)) cli::cli_abort("{.code ci_level} must be between 0 and 1 but {ci_level} was supplied.")
  if (!is.null(mid_deterioration)) {
    if (!is.numeric(mid_deterioration)) cli::cli_abort("{.code mid_deterioration} must be numeric but a {.code {typeof(mid_deterioration)}} was supplied.")
    if (mid_deterioration < 0) cli::cli_abort("{.code mid_deterioration} must be greater than 0 but {mid_deterioration} was supplied.")
  }
  if (cs_effect == "between") {
    if (cs_target == "individual") cli::cli_abort("A between subjects design can only be chosen if groups should be examined, but not individuals. Did you mean to set {.code target = \"group\"}?")
    if (missing(group)) cli::cli_abort("To calculate the difference between several groups, {.code group} must be set to a column containing a group identifier.")
    if (is.null(post)) cli::cli_abort("Argument {.code post} is missing with no default. The measurement for which groupwise differences should be calculated must be supplied.")
  }

  if (is.null(mid_deterioration)) mid_deterioration <- mid_improvement


  # Prepare the data
  if (cs_effect != "between") {
    datasets <- .prep_data(
      data = data,
      id = {{ id }},
      time = {{ time }},
      outcome = {{ outcome }},
      group = {{ group }},
      pre = {{ pre }},
      post = {{ post }}
    )
  } else {
    datasets <- data |>
      dplyr::select(
        id = {{ id }},
        time = {{ time }},
        outcome = {{ outcome }},
        group = {{  group }}
      )
  }


  # Prepend a class to enable method dispatch for RCI calculation
  prepend_classes <- c("cs_anchor", paste("cs", "anchor", cs_target, cs_effect, sep = "_"))
  class(datasets) <- c(prepend_classes, class(datasets))


  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )


  # Get the direction of a beneficial intervention effect
  if (rlang::arg_match(better_is) == "lower") direction <- -1 else direction <- 1


  # Check each participant's or group change relative to MID
  anchor_results <- calc_anchor(
    data = datasets,
    mid_improvement = mid_improvement,
    mid_deterioration = mid_deterioration,
    reference_group = reference_group,
    post = post,
    direction = direction,
    bayesian = bayesian,
    prior_scale = prior_scale,
    ci_level = ci_level
  )


  # Create the summary table for printing and exporting
  if (cs_target == "individual") {
    summary_table <- create_summary_table(
      x = anchor_results,
      data = datasets
    )

    class(anchor_results) <- c("tbl_df", "tbl", "data.frame")
  } else {
    summary_table <- NULL
    if (cs_effect == "within") class(datasets) <- "list" else class(datasets) <- c("tbl_df", "tbl", "data.frame")
  }


  # Put everything into a list
  output <- list(
    datasets = datasets,
    anchor_results = anchor_results,
    outcome = deparse(substitute(outcome)),
    n_obs = n_obs,
    mid_improvement = mid_improvement,
    mid_deterioration = mid_deterioration,
    direction = direction,
    bayesian = bayesian,
    prior_scale = prior_scale,
    summary_table = summary_table
  )


  # Return output
  class(output) <- c("cs_analysis", prepend_classes, class(output))
  output
}




#' Print Method for the Anchor-Based Approach for Individuals
#'
#' @param x An object of class `cs_anchor_individual_within`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#' cs_results
print.cs_anchor_individual_within <- function(x, ...) {
  summary_table <- x[["summary_table"]]
  mid_improvement <- x[["mid_improvement"]]
  mid_deterioration <- x[["mid_deterioration"]]
  direction <- x[["direction"]]

  if (direction == -1) dir_improvement <- "decrease" else dir_improvement <- "increase"
  if (direction == -1) dir_deterioration <- "increase" else dir_deterioration <- "decrease"

  outcome <- x[["outcome"]]

  if (mid_improvement == mid_deterioration) {
    pct_string <- "{.strong {mid_improvement} point} {dir_improvement} in instrument scores indicating a clinical significant improvement."
  } else {
    pct_string <- "{.strong {mid_improvement} point} {dir_improvement} in instrument scores indicating a clinical significant improvement and a {.strong {mid_deterioration} point} {dir_deterioration} in instrument scores indicating a clinical significant deterioration."
  }

  summary_table_formatted <- summary_table |>
    dplyr::rename_with(tools::toTitleCase)


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text(c("Individual anchor-based approach with a ", pct_string))
    cli::cat_line()
    cli::cli_verbatim(insight::export_table(summary_table_formatted))
  }
  output_fun()
}




#' Print Method for the Anchor-Based Approach for Groups (Within)
#'
#' @param x An object of class `cs_anchor_group_within`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#' cs_results
print.cs_anchor_group_within <- function(x, ...) {
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename(
      "CI-Level" = "ci",
      "[Lower" = "lower",
      "Upper]" = "upper",
      "Category" = "category"
    )
  if (.has_group(summary_table_formatted)) summary_table_formatted <- dplyr::rename(summary_table_formatted, "Group" = "group")
  if (!x[["bayesian"]]) summary_table_formatted <- dplyr::rename(summary_table_formatted, "Mean Difference" = "difference") else summary_table_formatted <- dplyr::rename(summary_table_formatted, "Median Difference" = "difference")

  mid_improvement <- x[["mid_improvement"]]
  direction <- x[["direction"]]

  if (direction == -1) dir_improvement <- "decrease" else dir_improvement <- "increase"
  if (direction == -1) dir_deterioration <- "increase" else dir_deterioration <- "decrease"

  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text("Groupwise anchor-based approach ({.strong within} groups) with a {.strong {mid_improvement} point} {dir_improvement} in instrument scores indicating a clinical significant improvement.")
    cli::cat_line()
    cli::cli_verbatim(insight::export_table(summary_table_formatted, align = "left"))
  }
  output_fun()
}




#' Print Method for the Anchor-Based Approach for Groups (Between)
#'
#' @param x An object of class `cs_anchor_group_between`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
print.cs_anchor_group_between <- function(x, ...) {
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename(
      "Group 1" = "reference",
      "Group 2" = "comparison",
      "CI-Level" = "ci",
      "[Lower" = "lower",
      "Upper]" = "upper",
      "Category" = "category",
      "n (1)" = "n_reference",
      "n (2)" = "n_comparison"
    )

  if (!x[["bayesian"]]) summary_table_formatted <- dplyr::rename(summary_table_formatted, "Mean Difference" = "difference") else summary_table_formatted <- dplyr::rename(summary_table_formatted, "Median Difference" = "difference")

  mid_improvement <- x[["mid_improvement"]]
  direction <- x[["direction"]]

  if (direction == -1) dir_improvement <- "decrease" else dir_improvement <- "increase"
  if (direction == -1) dir_deterioration <- "increase" else dir_deterioration <- "decrease"

  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text("Groupwise anchor-based approach ({.strong between} groups) with a {.strong {mid_improvement} point} {dir_improvement} in instrument scores indicating a clinical significant improvement.")
    cli::cat_line()
    cli::cli_verbatim(insight::export_table(summary_table_formatted, align = "left"))
  }
  output_fun()
}




#' Summary Method for the Anchor-Based Approach
#'
#' @param x An object of class `cs_anchor_individual_within`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_percentage(id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5)
#'
#' summary(cs_results)
summary.cs_anchor_individual_within <- function(x, ...) {
  # Get necessary information from object
  summary_table <- x[["summary_table"]] |>
    dplyr::rename_with(tools::toTitleCase)

  mid_improvement <- x[["mid_improvement"]]
  mid_deterioration <- x[["mid_deterioration"]]
  n_original <- cs_get_n(x, "original")[[1]]
  n_used <- cs_get_n(x, "used")[[1]]
  pct <- round(n_used / n_original, digits = 3) * 100
  direction <- x[["direction"]]

  if (direction == -1) dir_improvement <- "decrease" else dir_improvement <- "increase"
  if (direction == -1) dir_deterioration <- "increase" else dir_deterioration <- "decrease"

  outcome <- x[["outcome"]]

  if (mid_improvement == mid_deterioration) {
    pct_string <- "{.strong {mid_improvement} point} {dir_improvement} in instrument scores ({.strong {outcome}}) indicating a clinical significant improvement."
  } else {
    pct_string <- "{.strong {mid_improvement} point} {dir_improvement} in instrument scores ({.strong {outcome}}) indicating a clinical significant improvement and a {.strong {mid_deterioration} point} {dir_deterioration} in instrument scores indicating a clinical significant deterioration."
  }


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text(c("Individual anchor-based analysis of clinical significance with a ", pct_string))
    cli::cat_line()
    cli::cli_text("There were {.strong {n_original}} participants in the whole dataset of which {.strong {n_used}} {.strong ({pct}%)} could be included in the analysis.")
    cli::cat_line()
    cli::cli_h3("Individual Level Results")
    cli::cat_line()
    cli::cli_verbatim(insight::export_table(summary_table))
  }
  output_fun()
}




#' Summary Method for the Anchor-Based Approach for Groups (Within)
#'
#' @param x An object of class `cs_anchor_group_within`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_percentage(id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5)
#'
#' summary(cs_results)
summary.cs_anchor_group_within <- function(x, ...) {
  # Get necessary information from object
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename("Difference" = "difference", "CI-Level" = "ci", "[Lower" = "lower", "Upper]" = "upper", "Category" = "category")
  if (.has_group(summary_table_formatted)) summary_table_formatted <- dplyr::rename(summary_table_formatted, "Group" = "group")

  mid_improvement <- x[["mid_improvement"]]
  n_original <- cs_get_n(x, "original")[[1]]
  n_used <- cs_get_n(x, "used")[[1]]
  pct <- round(n_used / n_original, digits = 3) * 100
  direction <- x[["direction"]]

  if (direction == -1) dir_improvement <- "decrease" else dir_improvement <- "increase"
  if (direction == -1) dir_deterioration <- "increase" else dir_deterioration <- "decrease"

  outcome <- x[["outcome"]]


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text(c("Groupwise anchor-based analysis of clinical significance ({.strong within} groups) with a {.strong {mid_improvement} point} {dir_improvement} in instrument scores ({.strong {outcome}}) indicating a clinical significant improvement."))
    cli::cat_line()
    cli::cli_text("There were {.strong {n_original}} participants in the whole dataset of which {.strong {n_used}} {.strong ({pct}%)} could be included in the analysis.")
    cli::cat_line()
    cli::cli_h3("Group Level Results")
    cli::cat_line()
    cli::cli_verbatim(insight::export_table(summary_table_formatted, align = "left"))
  }
  output_fun()
}




#' Summary Method for the Anchor-Based Approach for Groups (Between)
#'
#' @param x An object of class `cs_anchor_group_between`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_percentage(id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5)
#'
#' summary(cs_results)
summary.cs_anchor_group_between <- function(x, ...) {
  # Get necessary information from object
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename(
      "Group 1" = "reference",
      "Group 2" = "comparison",
      "CI-Level" = "ci",
      "[Lower" = "lower",
      "Upper]" = "upper",
      "Category" = "category",
      "n (1)" = "n_reference",
      "n (2)" = "n_comparison"
    )

  if (!x[["bayesian"]]) summary_table_formatted <- dplyr::rename(summary_table_formatted, "Mean Difference" = "difference") else summary_table_formatted <- dplyr::rename(summary_table_formatted, "Median Difference" = "difference")

  mid_improvement <- x[["mid_improvement"]]
  direction <- x[["direction"]]

  if (direction == -1) dir_improvement <- "decrease" else dir_improvement <- "increase"
  if (direction == -1) dir_deterioration <- "increase" else dir_deterioration <- "decrease"

  outcome <- x[["outcome"]]


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text(c("Groupwise anchor-based analysis of clinical significance ({.strong between} groups) with a {.strong {mid_improvement} point} {dir_improvement} in instrument scores ({.strong {outcome}}) indicating a clinical significant improvement."))
    cli::cat_line()
    cli::cli_h3("Group Level Results")
    cli::cat_line()
    cli::cli_verbatim(insight::export_table(summary_table_formatted, align = "left"))
  }
  output_fun()
}
