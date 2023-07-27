#' Statistical Analysis of Clinical Significance
#'
#' @param m_functional Numeric, mean of functional population.
#' @param sd_functional Numeric, standard deviation of functional population
#' @param cutoff_method Cutoff method, Available are
#'   - `"JT"` (Jacobson & Truax, 1991, the default)
#'   - `"HA"` (Hageman & Arrindell, 1999)
#' @param cutoff_type Cutoff type. Available are `"a"`, `"b"`, and `"c"`.
#'   Defaults to `"a"` but `"c"` is usually recommended. For `"b"` and `"c"`,
#'   summary data from a functional population must be given with arguments
#'   `"m_functional"` and `"sd_functional"`.
#'
#' @inheritParams cs_distribution
#'
#' @references
#' - Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical approach to defining meaningful change in psychotherapy research. Journal of Consulting and Clinical Psychology, 59(1), 12–19. https://doi.org/10.1037//0022-006X.59.1.12
#' - Hageman, W. J., & Arrindell, W. A. (1999). Establishing clinically significant change: increment of precision and the distinction between individual and group level analysis. Behaviour Research and Therapy, 37(12), 1169–1193. https://doi.org/10.1016/S0005-7967(99)00032-7
#'
#' @return An S3 object of class `clinisig` and `cs_statistical`
#' @export
#'
#' @examples
#' claus_2020 |>
#'   cs_statistical(id, time, bdi, pre = 1, post = 4)
#'
#' # Different cutoff
#' claus_2020 |>
#'   cs_statistical(id, time, bdi, m_functional = 8, sd_functional = 7, pre = 1, post = 4, cutoff_type = "c")
#'
#' # Different method
#' claus_2020 |>
#'   cs_statistical(id, time, bdi, m_functional = 8, sd_functional = 7, pre = 1, post = 4, cutoff_type = "c", cutoff_method = "HA")
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
    data = datasets
  )


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
  class(output) <- c("clinisig", "cs_statistical", class(datasets), class(output))
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

  outcome <- x[["outcome"]]


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text("Statistical approach of clinical significance using the {.strong {cs_method}} method for calculating the population cutoff.")
    cli::cat_line()
    cli::cli_text("There were {.strong {n_original}} participants in the whole dataset of which {.strong {n_used}} {.strong ({pct}%)} could be included in the analysis.")
    cli::cat_line()
    cli::cli_h3("Individual Level Results")
    cli::cli_verbatim(insight::export_table(summary_table_formatted))
  }
  output_fun()
}