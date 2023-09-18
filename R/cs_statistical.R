#'Statistical Analysis of Clinical Significance
#'
#'@description `cs_statistical()` can be used to determine the clinical
#'  significance of intervention studies employing the statistical approach. For
#'  this, it will be assumed that the functional (non-clinical population) and
#'  patient (clinical population) scores form two distinct distributions on a
#'  continuum. `cs_statistical()` calculates a cutoff point between these two
#'  populations and counts, how many patients changed from the clinical to the
#'  functional population during intervention. Several methods for calculating
#'  this cutoff are available.
#'
#'@section Computational details: There are three available cutoff types, namely
#'  a, b, and c which can be used to "draw a line" or separate the functional
#'  and clinical population on a continuum. a as a cutoff is defined as the mean
#'  of the clinical population minus two times the standard deviation (SD) of
#'  the clinical population. b is defined as the mean of the functional
#'  population plus also two times the SD of the clinical population. This is
#'  true for "negative" outcomes, where a lower instrument score is desirable.
#'  For "positive" outcomes, where higher scores are beneficial, a is the mean
#'  of the clinical population plus 2 \eqn{\cdot} SD of the clinical population
#'  and b is mean of the functional population minus 2 \eqn{\cdot} SD of the
#'  clinical population. The summary statistics for the clinical population are
#'  estimated from the provided data at pre measurement.
#'
#'  c is defined as the midpoint between both populations based on their
#'  respective mean and SD. In order to calculate b and c, descriptive
#'  statistics for the functional population must be provided.
#'
#'@section Categories: Individual patients can be categorized into one of the
#'  following groups:
#'  - Improved, i.e., one changed from the clinical to the functional population
#'  - Unchanged, i.e., one can be seen as a member of the same population pre
#'  and post intervention
#'  - Deteriorated, i.e., one changed from the functional to the clinical
#'  population during intervention
#'
#'
#'@inheritSection cs_distribution Data preparation
#'
#'@inheritParams cs_distribution
#'@param m_functional Numeric, mean of functional population.
#'@param sd_functional Numeric, standard deviation of functional population
#'@param cutoff_method Cutoff method, Available are
#'    - `"JT"` (Jacobson & Truax, 1991, the default)
#'    - `"HA"` (Hageman & Arrindell, 1999)
#'@param cutoff_type Cutoff type. Available are `"a"`, `"b"`, and `"c"`.
#'  Defaults to `"a"` but `"c"` is usually recommended. For `"b"` and `"c"`,
#'  summary data from a functional population must be given with arguments
#'  `m_functional` and `sd_functional`.
#'
#'
#'@references
#'  - Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical approach to defining meaningful change in psychotherapy research. Journal of Consulting and Clinical Psychology, 59(1), 12–19. https://doi.org/10.1037//0022-006X.59.1.12
#'  - Hageman, W. J., & Arrindell, W. A. (1999). Establishing clinically significant change: increment of precision and the distinction between individual and group level analysis. Behaviour Research and Therapy, 37(12), 1169–1193. https://doi.org/10.1016/S0005-7967(99)00032-7
#'
#'@family main
#'
#'@return An S3 object of class `cs_analysis` and `cs_statistical`
#'@export
#'
#' @examples
#' # By default, cutoff type "a" is used
#' cs_results <- claus_2020 |>
#'   cs_statistical(id, time, hamd, pre = 1, post = 4)
#'
#' cs_results
#' summary(cs_results)
#' plot(cs_results)
#'
#'
#' # You can choose a different cutoff type but need to provide additional
#' # population summary statistics for the functional population
#' cs_results_c <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
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
#' # You can use a different method to calculate the cutoff
#' cs_results_ha <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     reliability = 0.80,
#'     cutoff_type = "c",
#'     cutoff_method = "HA"
#'   )
#'
#' cs_results_ha
#' summary(cs_results_ha)
#' plot(cs_results_ha)
#'
#'
#' # And you can group the analysis by providing a grouping variable from the data
#' cs_results_grouped <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     cutoff_type = "c",
#'     group = treatment
#'   )
#'
#' cs_results_grouped
#' summary(cs_results_grouped)
#' plot(cs_results_grouped)
cs_statistical <- function(data,
                           id,
                           time,
                           outcome,
                           group = NULL,
                           pre = NULL,
                           post = NULL,
                           m_functional = NULL,
                           sd_functional = NULL,
                           reliability = NULL,
                           better_is = c("lower", "higher"),
                           cutoff_method = c("JT", "HA"),
                           cutoff_type = c("a", "b", "c"),
                           significance_level = 0.05) {
  # Check arguments
  cs_method <- rlang::arg_match(cutoff_method)
  cut_type <- rlang::arg_match(cutoff_type)
  if (missing(id)) cli::cli_abort("Argument {.code id} is missing with no default. A column containing patient-specific IDs must be supplied.")
  if (missing(time)) cli::cli_abort("Argument {.code time} is missing with no default. A column identifying the individual measurements must be supplied.")
  if (missing(outcome)) cli::cli_abort("Argument {.code outcome} is missing with no default. A column containing the outcome must be supplied.")
  if (cs_method == "HA") {
    if (is.null(reliability)) cli::cli_abort("Argument {.code reliability} is missing with no default. An instrument reliability must be supplied.")
    if (!is.null(reliability) & !is.numeric(reliability)) cli::cli_abort("{.code reliability} must be numeric but a {.code {typeof(reliability)}} was supplied.")
    if (!is.null(reliability) & !dplyr::between(reliability, 0, 1)) cli::cli_abort("{.code reliability} must be between 0 and 1 but {reliability} was supplied.")
  } else {
    if (!is.null(reliability)) cli::cli_alert_info("A reliability for the JT approach to calculating a population cutoff is not needed and will be ignored.")
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
  class(datasets) <- c(paste0("cs_", tolower(cs_method)), class(datasets))


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


  # Create the summary table for printing and exporting
  summary_table <- create_summary_table(
    x = cutoff_results,
    data = datasets,
    method = cs_method
  )


  class(cutoff_results) <- "list"

  # Put everything into a list
  output <- list(
    datasets = datasets,
    cutoff_results = cutoff_results,
    outcome = deparse(substitute(outcome)),
    n_obs = n_obs,
    method = cs_method,
    reliability = reliability,
    critical_value = critical_value,
    summary_table = summary_table
  )


  # Return output
  class(output) <- c("cs_analysis", "cs_statistical", class(datasets), class(output))
  output
}




#' Print Method for the Statistical Approach
#'
#' @param x An object of class `cs_distribution`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_statistical(id, time, hamd, pre = 1, post = 4, m_functional = 8, sd_functional = 7)
#' cs_results
print.cs_statistical <- function(x, ...) {
  summary_table <- x[["summary_table"]]
  cs_method <- x[["method"]]

  summary_table_formatted <- summary_table |>
    dplyr::rename_with(tools::toTitleCase)


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text("Statistical approach using the {.strong {cs_method}} method.")
    cli::cat_line()
    cli::cli_verbatim(insight::export_table(summary_table_formatted))
  }
  output_fun()
}



#' Summary Method for the Statistical Approach
#'
#' @param x An object of class `cs_distribution`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_statistical(id, time, hamd, pre = 1, post = 4, m_functional = 8, sd_functional = 7)
#'
#' summary(cs_results)
summary.cs_statistical <- function(x, ...) {
  # Get necessary information from object
  summary_table <- x[["summary_table"]]
  summary_table_formatted <- summary_table |>
    dplyr::rename_with(tools::toTitleCase)

  cs_method <- x[["method"]]
  n_original <- cs_get_n(x, "original")[[1]]
  n_used <- cs_get_n(x, "used")[[1]]
  pct <- round(n_used / n_original, digits = 3) * 100
  cutoff_info <- cs_get_cutoff(x, with_descriptives = TRUE)
  cutoff_type <- cutoff_info[["type"]]
  cutoff_value <- round(cutoff_info[["value"]], 2)
  cutoff_descriptives <- cutoff_info[, 1:4] |>
    dplyr::rename("M Clinical" = "m_clinical", "SD Clinical" = "sd_clinical", "M Functional" = "m_functional", "SD Functional" = "sd_functional") |>
    insight::export_table(missing = "---", )

  outcome <- x[["outcome"]]


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text("Statistical approach of clinical significance using the {.strong {cs_method}} method for calculating the population cutoff.")
    cli::cat_line()
    cli::cli_text("There were {.strong {n_original}} participants in the whole dataset of which {.strong {n_used}} {.strong ({pct}%)} could be included in the analysis.")
    cli::cat_line()
    cli::cli_text("The cutoff type was {.strong {cutoff_type}} with a value of {.strong {cutoff_value}} based on the following sumamry statistics:")
    cli::cat_line()
    cli::cli_h3("Population Characteristics")
    cli::cli_verbatim(cutoff_descriptives)
    cli::cat_line()
    cli::cli_h3("Individual Level Results")
    cli::cli_verbatim(insight::export_table(summary_table_formatted))
  }
  output_fun()
}
