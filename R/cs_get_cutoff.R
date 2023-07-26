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
cs_get_cutoff <- function(x, with_descriptives = FALSE) {
  if (!inherits(x, "clinisig")) cli::cli_abort("The supplied object must be of class {.code clinisig}.")

  cutoff_info <- x[["cutoff_results"]][["info"]] |>
    purrr::map(\(x) ifelse(is.null(x), NA, x)) |>
    as_tibble()

  if (!with_descriptives) cutoff_info |> select(type, value) else cutoff_info
}
