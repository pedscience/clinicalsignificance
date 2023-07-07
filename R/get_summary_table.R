#' Get A Summary Table From A clinisig Object
#'
#' Retrieve the summary table in a tidy tibble format. This is especially useful
#' to plot the results or conduct sensitivity analyses.
#'
#' @inheritParams cs_get_data
#' @param which Which level of summary table to return. This is only necessary
#'   for method `"HA"` since two summary tables are reported. Available are
#'   - `individual`, the default
#'   - `group`, group level results according to Hageman & Arrindell (1999)
#'
#' @importFrom rlang abort
#'
#' @return A tibble with clinical significance categories
#' @export
#'
#' @references
#' - Hageman, W. J., & Arrindell, W. A. (1999). Establishing clinically significant change: increment of precision and the distinction between individual and group level analysis. Behaviour Research and Therapy, 37(12), 1169–1193. https://doi.org/10.1016/S0005-7967(99)00032-7
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
#' results_ha <- jacobson_1989  |>
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = gds,
#'     pre = "pre",
#'     reliability = 0.80,
#'     m_functional = 30,
#'     sd_functional = 10,
#'     type = "c",
#'     method = "HA"
#'   )
#'
#' get_summary_table(results)
#' get_summary_table(results_ha)
#' get_summary_table(results_ha, which = "group")
get_summary_table <- function(x, which = c("individual", "group")) {
  assert_class(x, "clinisig")

  which_table <- arg_match(which)
  clinisig_method <- get_method(x)
  if (clinisig_method != "HA" & which_table == "group") abort("Group level results can only be exported for method HA.")

  if (clinisig_method == "HA") {
    if (which_table == "group") x[["summary"]][[2]] else x[["summary"]][[1]]
  } else {
    x[["summary"]]
  }
}
