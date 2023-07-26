#' Get Descriptives Used In The Cutoff Calculation
#'
#' @param x A clinisig object
#'
#' @return A tibble with means and standard deviations of the clinical and
#'   functional population
#' @export
cs_get_cutoff_descriptives <- function(x) {
  if (!inherits(x, "clinisig")) cli::cli_abort("The supplied object must be of class {.code clinisig}.")
  if (!inherits(x, "cs_statistical")) cli::cli_abort("The supplied object does not contain an analysis for which population cutoffs have been calculated.")

  x[["cutoff_results"]][["info"]] |>
    as_tibble() |>
    select(-type, -value)
}
