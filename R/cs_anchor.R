#' Anchor-Based Analysis of Clinical Significance
#'
#' @inheritParams cs_distribution
#' @param mid_improvement Numeric, change that indicates a clinically
#'   significant improvement
#' @param mid_deterioration Numeric, change that indicates a clinically
#'   significant deterioration
#'
#'
#' @return An S3 object of class `cs_analysis` and `cs_anchor`
#' @export
#'
#' @examples
#' claus_2020 |>
#'   cs_percentage(id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5)
#'
#'
#' # Different thresholds for improvement and deterioration
#' claus_2020 |>
#'   cs_percentage(id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5, pct_deterioration = 0.3)
cs_anchor <- function(data,
                      id,
                      time,
                      outcome,
                      group = NULL,
                      pre = NULL,
                      post = NULL,
                      mid_improvement = NULL,
                      mid_deterioration = NULL,
                      better_is = c("lower", "higher"),
                      target = c("individual", "group"),
                      effect = c("within", "between"),
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
  if (!is.null(mid_deterioration)) {
    if (!is.numeric(mid_deterioration)) cli::cli_abort("{.code mid_deterioration} must be numeric but a {.code {typeof(mid_deterioration)}} was supplied.")
    if (!dplyr::between(mid_deterioration, 0, 1)) cli::cli_abort("{.code mid_deterioration} must be between 0 and 1 but {mid_deterioration} was supplied.")
  }

  if (is.null(mid_deterioration)) mid_deterioration <- mid_improvement


  # Prepare the data
  datasets <- .prep_data(
    data = data,
    id = {{ id }},
    time = {{ time }},
    outcome = {{ outcome }},
    group = {{ group }},
    pre = {{ pre }},
    post = {{ post }}
  )


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
    direction = direction,
    ci_level = ci_level
  )



  # Create the summary table for printing and exporting
  if (cs_target == "individual") {
    summary_table <- create_summary_table(
      anchor_results = anchor_results,
      data = datasets
    )

    class(anchor_results) <- c("tbl_df", "tbl", "data.frame")
  } else {
    summary_table <- NULL
    class(datasets) <- "list"
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
    pct_string <- "{.strong {mid_improvement} point} {dir_improvement} in instrument scores indicating a clinical significant improvement and a {.strong {pct_deterioration} point} {dir_deterioration} in instrument scores indicating a clinical significant deterioration."
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
print.cs_anchor_group_within <- function(x, ...) {
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename("Mean Intervention Effect" = "mean_difference", "CI-Level" = "ci", "[Lower" = "lower", "Upper]" = "upper", "Category" = "category")
  if (.has_group(summary_table_formatted)) summary_table_formatted <- dplyr::rename(summary_table_formatted, "Group" = "group")

  mid_improvement <- x[["mid_improvement"]]
  direction <- x[["direction"]]

  if (direction == -1) dir_improvement <- "decrease" else dir_improvement <- "increase"
  if (direction == -1) dir_deterioration <- "increase" else dir_deterioration <- "decrease"

  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text("Groupwise anchor-based approach with a {.strong {mid_improvement} point} {dir_improvement} in instrument scores indicating a clinical significant improvement.")
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
    pct_string <- "{.strong {mid_improvement} point} {dir_improvement} in instrument scores ({.strong {outcome}}) indicating a clinical significant improvement and a {.strong {pct_deterioration} point} {dir_deterioration} in instrument scores indicating a clinical significant deterioration."
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
summary.cs_anchor_group_within <- function(x, ...) {
  # Get necessary information from object
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename("Mean Intervention Effect" = "mean_difference", "CI-Level" = "ci", "[Lower" = "lower", "Upper]" = "upper", "Category" = "category")
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
    cli::cli_text(c("Groupwise anchor-based analysis of clinical significance (within) with a {.strong {mid_improvement} point} {dir_improvement} in instrument scores ({.strong {outcome}}) indicating a clinical significant improvement."))
    cli::cat_line()
    cli::cli_text("There were {.strong {n_original}} participants in the whole dataset of which {.strong {n_used}} {.strong ({pct}%)} could be included in the analysis.")
    cli::cat_line()
    cli::cli_h3("Group Level Results")
    cli::cat_line()
    cli::cli_verbatim(insight::export_table(summary_table_formatted, align = "left"))
  }
  output_fun()
}
