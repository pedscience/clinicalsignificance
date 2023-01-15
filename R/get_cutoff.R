#' Get Used Cutoff And Type From A clinisig Object
#'
#' @param x A clinisig object
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
#' results <- jacobson_1989 |>
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = gds,
#'     pre = "pre",
#'     reliability = 0.80,
#'     m_functional = 30,
#'     sd_functional = 10,
#'     type = "c"
#'   )
#'
#' get_cutoff(results)
get_cutoff <- function(x, with_descriptives = FALSE) {
  assert_class(x, "clinisig")

  cutoff_info <- x[["cutoff"]][["info"]] |>
    as_tibble()

  if (!with_descriptives) {
    cutoff_info |>
      select(type, value)
  } else {
    cutoff_info
  }
}
