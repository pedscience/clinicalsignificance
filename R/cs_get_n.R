#' Get Number Of Participants From A cs_analysis Object
#'
#' @description With `cs_get_n()` one can extract the number of participants
#'   used in a clinical significance analysis from a `cs_analysis`object. This
#'   may depend on the clinical significance approach and if missing values were
#'   present in the dataset. For all individual analyses, missing values are
#'   handled by list-wise deletion. Consequently, individuals with a missing pre
#'   or post intervention score will be omitted from the analyses.
#'
#' @param x A cs_analysis object
#' @param which Which n should be returned? Available options are
#' - `"all"`, (the default) returns the number of participants in both, the
#'   original and used data set
#' - `"original"`, number of participants in the original dataset
#' - `"used"`, number of participants in the used data set, so after conversion
#'   to wide format and omitting cases with missing values
#'
#' @family get
#'
#' @return A tibble with number of participants
#' @export
#'
#' @examples
#' # n can be extracted for every approach
#' cs_results_anchor <- claus_2020 |>
#'   cs_anchor(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     mid_improvement = 9
#'   )
#'
#' cs_results_distribution <- claus_2020 |>
#'   cs_distribution(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     reliability = 0.80
#'   )
#'
#' cs_results_statistical <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     cutoff_type = "c"
#'   )
#'
#' cs_results_combined <- claus_2020 |>
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
#' cs_results_percentage <- claus_2020 |>
#'   cs_percentage(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     pct_improvement = 0.3
#'   )
#'
#'
#' cs_get_n(cs_results_anchor)
#' cs_get_n(cs_results_distribution)
#' cs_get_n(cs_results_statistical)
#' cs_get_n(cs_results_combined)
#' cs_get_n(cs_results_percentage)
#'
#'
#' # Get your desired n
#' cs_get_n(cs_results_anchor, which = "all")
#' cs_get_n(cs_results_anchor, which = "original")
#' cs_get_n(cs_results_anchor, which = "used")
cs_get_n <- function(x, which = "all") {
  .check_class(x)


  if (which == "all") {
    x[["n_obs"]] |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        percent_used = n_used / n_original
      )
  } else if (which == "original") {
    dplyr::tibble(
      n_original = x[["n_obs"]][["n_original"]]
    )
  } else if (which == "used") {
    dplyr::tibble(
      n_used = x[["n_obs"]][["n_used"]]
    )
  }
}
