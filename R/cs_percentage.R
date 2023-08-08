#' Percentage-Change Analysis of Clinical Significance
#'
#' @inheritParams cs_distribution
#'
#'
#' @return An S3 object of class `clinisig` and `cs_percentage`
#' @export
#'
#' @examples
#' claus_2020 |>
#'   cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#'
#'
#' # Different RCI method
#' claus_2020 |>
#'   cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.8, rci_method = "EN")
cs_percentage <- function(data,
                          id,
                          time,
                          outcome,
                          group = NULL,
                          pre = NULL,
                          post = NULL,
                          pct_improvement = NULL,
                          pct_deterioration = NULL,
                          better_is = c("lower", "higher")) {
  # Check arguments
  if (missing(id)) cli::cli_abort("Argument {.code id} is missing with no default. A column containing patient-specific IDs must be supplied.")
  if (missing(time)) cli::cli_abort("Argument {.code time} is missing with no default. A column identifying the individual measurements must be supplied.")
  if (missing(outcome)) cli::cli_abort("Argument {.code outcome} is missing with no default. A column containing the outcome must be supplied.")

  if (is.null(pct_deterioration)) pct_deterioration <- pct_improvement


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
  class(datasets) <- c("cs_percentage", class(datasets))


  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )


  # Get the direction of a beneficial intervention effect
  if (rlang::arg_match(better_is) == "lower") direction <- -1 else direction <- 1


  # Determine RCI and check each participant's change relative to it
  pct_results <- calc_percentage(
    data = datasets[["data"]],
    pct_improvement = pct_improvement,
    pct_deterioration = pct_deterioration,
    direction = direction
  )



  # Create the summary table for printing and exporting
  summary_table <- create_summary_table(
    pct_results = pct_results,
    data = datasets
  )


  class(pct_results) <- c("tbl_df", "tbl", "data.frame")

  # Put everything into a list
  output <- list(
    datasets = datasets,
    pct_results = pct_results,
    outcome = deparse(substitute(outcome)),
    n_obs = n_obs,
    pct_improvement = pct_improvement,
    pct_deterioration = pct_deterioration,
    summary_table = summary_table
  )


  # Return output
  class(output) <- c("clinisig", "cs_percentage", class(output))
  output
}


calc_percentage <- function(data, pct_improvement, pct_deterioration, direction) {
  out <- data |>
    dplyr::mutate(
      pct_change   = change / pre,
      improved     = direction * pct_change >= pct_improvement,
      deteriorated = direction * pct_change <= -pct_deterioration,
      unchanged = !improved & !deteriorated
    )

  class(out) <- c("cs_percentage", class(out))
  out
}




#' Print Method for the Percentange-Change Approach
#'
#' @param x An object of class `cs_percentage`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#' cs_results
print.cs_percentage <- function(x, ...) {
  summary_table <- x[["summary_table"]]
  pct_improvement <- sprintf("%.2f", x[["pct_improvement"]])
  pct_deterioration <- sprintf("%.2f", x[["pct_deterioration"]])

  if (pct_improvement == pct_deterioration) {
    pct_string <- "{.strong {pct_improvement}%} change indicating a clinical significant improvement."
  } else {
    pct_string <- "{.strong {pct_improvement}%} change indicating a clinical significant improvement and {.strong {pct_deterioration}%} change indicating a clinical significant deterioration."
  }

  summary_table_formatted <- summary_table |>
    dplyr::rename_with(tools::toTitleCase)


  # Print output
  output_fun <- function() {
    cli::cli_h2("Clinical Significance Results")
    cli::cli_text(c("Percentage-change approach with ", pct_string))
    cli::cat_line()
    cli::cli_verbatim(insight::export_table(summary_table_formatted))
  }
  output_fun()
}
