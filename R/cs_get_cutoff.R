#' Get Used Cutoff And Type From A cs_analysis Object
#'
#' @param x A cs_analysis object
#' @param with_descriptives Logical indicating whether you want to retrieve only
#'   the cutoff type and value or the summary statistics on which it is based
#'   on. The default is `FALSE`.
#'
#' @importFrom dplyr select as_tibble
#'
#' @return A tibble with cutoff information
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_statistical(id, time, bdi, pre = 1, post = 4, m_functional = 8, sd_functional = 8, cutoff_type = "c")
#'
#' cs_get_cutoff(cs_results)
#' cs_get_cutoff(cs_results, with_descriptives = TRUE)
cs_get_cutoff <- function(x, with_descriptives = FALSE) {
  .check_class(x)
  if (!inherits(x, "cs_statistical") & !inherits(x, "cs_combined")) cli::cli_abort("There was no statistical population cutoff calculated for this clinical significance method.")

  cutoff_info <- x[["cutoff_results"]][["info"]] |>
    purrr::map(\(x) ifelse(is.null(x), NA, x)) |>
    as_tibble()

  if (!with_descriptives) cutoff_info |> select(type, value) else cutoff_info
}
