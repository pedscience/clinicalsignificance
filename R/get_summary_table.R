#' Get A Summary Table From A clinisig Object
#'
#' @inheritParams get_data
#' @param which Which level of summary table to return. This is only and issue
#'   for method `"HA"` since two summary tables are reported.
#'
#' @importFrom rlang abort
#'
#' @return A tibble with clinical significance categories
#' @export
get_summary_table <- function(x, which = c("individual", "group")) {
  assert_class(x, "clinisig")

  which_table <- arg_match(which)
  clinisig_method <- get_clinical_significance_method(x)
  if (clinisig_method != "HA" & which_table == "group") abort("Group level results can only be exported for method HA.")

  if (clinisig_method == "HA") {
    if (which_table == "group") x[["summary"]][[2]] else x[["summary"]][[1]]
  } else {
    x[["summary"]]
  }
}
