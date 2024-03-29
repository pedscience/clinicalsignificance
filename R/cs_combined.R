#' Combined Analysis of Clinical Significance
#'
#' @description `cs_combined()` can be used to determine the clinical
#'   significance of intervention studies employing the combination of the
#'   distribution-based and statistical approach. For this, it will be assumed
#'   that the functional (non-clinical population) and patient (clinical
#'   population) scores form two distinct distributions on a continuum.
#'   `cs_combined()` calculates a cutoff point between these two populations as
#'   well as a reliable change index (RCI) based on a provided instrument
#'   reliability estimate and counts, how many of those patients that showed a
#'   reliable change (that is likely to be not due to measurement error)
#'   switched from the clinical to the functional population during
#'   intervention. Several methods for calculating the cutoff and RCI are
#'   available.
#'
#' @inheritSection cs_statistical Computational details
#' @inheritSection cs_distribution Computational details
#'
#' @section Categories: Each individual's change can then be categorized into
#'   the following groups:
#' - Recovered, i.e., the individual showed a reliable change in the beneficial direction and changed from the clinical to the functional population
#' - Improved, i.e., the individual showed a reliable change in the beneficial direction but did not change populations
#' - Unchanged, i.e., the individual showed no reliable change
#' - Deteriorated, i.e., the individual showed a reliable change in the disadvantageous direction but did not change populations
#' - Harmed, i.e., the individual showed a reliable change in the disadvantageous direction and switched from the functional to the clinincal population
#'
#' @inheritSection cs_distribution Data preparation
#'
#' @inheritParams cs_distribution
#' @inheritParams cs_statistical
#' @inheritParams cs_anchor
#'
#' @family main
#'
#' @return An S3 object of class `cs_analysis` and `cs_combined`
#' @export
#'
#' @examples
# In this case, cutoff "a" is chosen by default
#' cs_results <- claus_2020 |>
#'   cs_combined(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     reliability = 0.80
#'   )
#'
#' cs_results
#' summary(cs_results)
#' plot(cs_results)
#'
#'
#' # You can choose a different cutoff but must provide summary statistics for the
#' # functional population
#' cs_results_c <- claus_2020 |>
#'   cs_combined(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     reliability = 0.80,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     cutoff_type = "c"
#'   )
#'
#' cs_results_c
#' summary(cs_results_c)
#' plot(cs_results_c)
#'
#'
#' # You can group the analysis by providing a grouping variable in the data
#' cs_results_grouped <- claus_2020 |>
#'   cs_combined(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     group = treatment,
#'     reliability = 0.80,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     cutoff_type = "c"
#'   )
#'
#' cs_results_grouped
#' summary(cs_results_grouped)
#' plot(cs_results_grouped)
cs_combined <- function(data,
                        id,
                        time,
                        outcome,
                        group = NULL,
                        pre = NULL,
                        post = NULL,
                        mid_improvement = NULL,
                        mid_deterioration = NULL,
                        reliability = NULL,
                        reliability_post = NULL,
                        m_functional = NULL,
                        sd_functional = NULL,
                        better_is = c("lower", "higher"),
                        rci_method = c("JT", "GLN", "HLL", "EN", "NK", "HA", "HLM"),
                        cutoff_type = c("a", "b", "c"),
                        significance_level = 0.05) {
  # Argument checks
  cs_method <- rlang::arg_match(rci_method)
  cut_type <- rlang::arg_match(cutoff_type)
  if (missing(id)) cli::cli_abort("Argument {.code id} is missing with no default. A column containing patient-specific IDs must be supplied.")
  if (missing(time)) cli::cli_abort("Argument {.code time} is missing with no default. A column identifying the individual measurements must be supplied.")
  if (missing(outcome)) cli::cli_abort("Argument {.code outcome} is missing with no default. A column containing the outcome must be supplied.")
  if (is.null(mid_improvement) & cs_method != "HLM") {
    if (is.null(reliability)) cli::cli_abort("Argument {.code reliability} is missing with no default. An instrument reliability must be supplied.")
    if (!is.null(reliability) & !is.numeric(reliability)) cli::cli_abort("{.code reliability} must be numeric but a {.code {typeof(reliability)}} was supplied.")
    if (!is.null(reliability) & !dplyr::between(reliability, 0, 1)) cli::cli_abort("{.code reliability} must be between 0 and 1 but {reliability} was supplied.")

  }
  if (cut_type %in% c("b", "c")) {
    if (is.null(m_functional) | is.null(sd_functional)) cli::cli_abort("For cutoffs {.code b} and {.code c}, mean and standard deviation for a functional population must be provided via {.code m_functional} and {.code sd_functional}")
    if ((!is.null(m_functional) & !is.numeric(m_functional)) | (!is.null(sd_functional) & !is.numeric(sd_functional))) cli::cli_abort("The mean and standard deviation supplied with {.code m_functional} and {.code sd_functional} must be numeric.")
  }

  # Prepare the data
  datasets <- .prep_data(
    data = data,
    id = {{ id }},
    time = {{ time }},
    outcome = {{ outcome }},
    group = {{ group }},
    pre = {{ pre }},
    post = {{ post }},
    method = cs_method
  )


  # Prepend a class to enable method dispatch for RCI calculation
  if (!is.null(mid_improvement)) class(datasets) <- c("cs_anchor_individual_within", class(datasets)) else class(datasets) <- c(paste0("cs_", tolower(cs_method)), class(datasets))
  if (!is.null(mid_improvement)) cs_method <- "anchor-based"


  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )


  # Calculate relevant summary statistics for the chosen RCI method
  m_pre <- mean(datasets[["data"]][["pre"]])
  sd_pre <- stats::sd(datasets[["data"]][["pre"]])
  if (cs_method %in% c("HLL", "HA")) {
    m_post <- mean(datasets[["data"]][["post"]])
    sd_post <- stats::sd(datasets[["data"]][["post"]])
  }


  # Get the direction of a beneficial intervention effect
  if (rlang::arg_match(better_is) == "lower") direction <- -1 else direction <- 1


  # Determine critical RCI value based on significance level
  if (cs_method != "HA") critical_value <- stats::qnorm(1 - significance_level/2) else critical_value <- stats::qnorm(1 - significance_level)


  if (is.null(mid_improvement)) {
    # Determine RCI and check each participant's change relative to it
    rci_results <- calc_rci(
      data = datasets,
      m_pre = m_pre,
      m_post = m_post,
      sd_pre = sd_pre,
      sd_post = sd_post,
      reliability = reliability,
      reliability_post = reliability_post,
      direction = direction,
      critical_value = critical_value
    )
  } else {
    # Check each participant's or group change relative to MID
    if (is.null(mid_deterioration)) mid_deterioration <- mid_improvement

    rci_results <- calc_anchor(
      data = datasets,
      mid_improvement = mid_improvement,
      mid_deterioration = mid_deterioration,
      direction = direction
    )
  }


  # Calculate the cutoff value and check each patient's change relative to it
  cutoff_results <- calc_cutoff_from_data(
    data = datasets,
    m_clinical = m_pre,
    sd_clinical = sd_pre,
    m_functional = m_functional,
    sd_functional = sd_functional,
    m_post = m_post,
    sd_post = sd_post,
    reliability = reliability,
    type = cut_type,
    direction = direction,
    critical_value = critical_value
  )

  class(rci_results) <- c("cs_combined", "list")



  # Create the summary table for printing and exporting
  summary_table <- create_summary_table(
    x = rci_results,
    cutoff_results = cutoff_results,
    data = datasets,
    method = cs_method,
    r_dd = rci_results[["r_dd"]],
    se_measurement = rci_results[["se_measurement"]],
    cutoff = cutoff_results[["info"]][["value"]],
    sd_post = sd_post,
    direction = direction
  )


  class(rci_results) <- "list"
  class(cutoff_results) <- "list"


  # Put everything into a list
  output <- list(
    datasets = datasets,
    cutoff_results = cutoff_results,
    rci_results = rci_results,
    outcome = deparse(substitute(outcome)),
    n_obs = n_obs,
    method = cs_method,
    mid_improvement = mid_improvement,
    mid_deterioration = mid_deterioration,
    direction = direction,
    reliability = reliability,
    critical_value = critical_value,
    summary_table = summary_table
  )


  # Return output
  class(output) <- c("cs_analysis", "cs_combined", class(datasets), class(output))
  output
}




#' Print Method for the Combined Approach
#'
#' @param x An object of class `cs_combined`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_combined(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#'
#' cs_results
print.cs_combined <- function(x, ...) {
  individual_summary_table <- x[["summary_table"]][["individual_level_summary"]]
  group_summary_table <- x[["summary_table"]][["group_level_summary"]]
  cs_method <- x[["method"]]

  individual_summary_table_formatted <- individual_summary_table |>
    dplyr::rename_with(tools::toTitleCase)

  if (cs_method == "HA") {
    group_summary_table_formatted <- group_summary_table |>
      dplyr::rename_with(tools::toTitleCase)
  }


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text("Combined approach using the {.strong {cs_method}} and {.strong statistical} approach.")
    cli::cat_line()
    if (cs_method != "HA") {
      cli::cli_verbatim(insight::export_table(individual_summary_table_formatted))
    } else {
      cli::cli_text("Individual Level Summary")
      cli::cli_verbatim(insight::export_table(individual_summary_table_formatted))
      cli::cat_line()
      cli::cli_text("Groupcs Level Summary")
      cli::cli_verbatim(insight::export_table(group_summary_table_formatted))
    }
  }
  output_fun()
}




#' Summary Method for the Combined Approach
#'
#' @param object An object of class `cs_combined`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_combined(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#'
#' summary(cs_results)
summary.cs_combined <- function(object, ...) {
  # browser()
  # Get necessary information from object
  summary_table_formatted <- object[["summary_table"]][["individual_level_summary"]] |>
    dplyr::rename_with(tools::toTitleCase) |>
    insight::export_table()

  cs_method <- object[["method"]]
  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]
  pct <- round(n_used / n_original, digits = 3) * 100
  cutoff_info <- cs_get_cutoff(object, with_descriptives = TRUE)
  cutoff_type <- cutoff_info[["type"]]
  cutoff_value <- round(cutoff_info[["value"]], 2)
  cutoff_descriptives <- cutoff_info[, 1:4] |>
    dplyr::rename("M Clinical" = "m_clinical", "SD Clinical" = "sd_clinical", "M Functional" = "m_functional", "SD Functional" = "sd_functional") |>
    insight::export_table(missing = "---", )
  mid <- object[["mid_improvement"]]

  if (cs_method == "HA") {
    group_summary_table <- object[["summary_table"]][["group_level_summary"]] |>
      dplyr::rename_with(tools::toTitleCase) |>
      insight::export_table()
  }

  outcome <- object[["outcome"]]
  if (cs_method == "anchor-based") {
    reliability_summary <- "The outcome was {.strong {outcome}} and the MID was set to {.strong {mid}}."
  } else if (cs_method != "NK") {
    reliability <- cs_get_reliability(object)[[1]]
    reliability_summary <- "The outcome was {.strong {outcome}} and the reliability was set to {.strong {reliability}}."
  } else {
    reliability_pre <- cs_get_reliability(object)[[1]]
    reliability_post <- cs_get_reliability(object)[[2]]
    reliability_summary <- "The outcome was {.strong {outcome}} and the reliability was set to {.strong {reliability_pre}} (pre intervention) and {.strong {reliability_post}} (post intervention)."
  }


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text("Combined analysis of clinical significance using the {.strong {cs_method}} and {.strong statistical} approach method for calculating the RCI and population cutoffs.")
    cli::cat_line()
    cli::cli_text("There were {.strong {n_original}} participants in the whole dataset of which {.strong {n_used}} {.strong ({pct}%)} could be included in the analysis.")
    cli::cat_line()
    cli::cli_text(reliability_summary)
    cli::cat_line()
    cli::cli_text("The cutoff type was {.strong {cutoff_type}} with a value of {.strong {cutoff_value}} based on the following sumamry statistics:")
    cli::cat_line()
    cli::cli_h3("Population Characteristics")
    cli::cli_verbatim(cutoff_descriptives)
    cli::cat_line()

    cli::cli_h3("Individual Level Results")
    cli::cli_verbatim(summary_table_formatted)
    if (cs_method == "HA") {
      cli::cat_line()
      cli::cli_h3("Group Level Results")
      cli::cli_verbatim(group_summary_table)
    }
  }
  output_fun()
}
