#' Get the employed clinical significance method
#'
#' You may choose between several methods to conduct clinical significance
#' analyses. This function can retrieve the employed method of a clinisig
#' object.
#'
#' @param x A clinisig object
#'
#' @return A string
#' @export
#'
#' @examples
#' results <- jacobson_1989 %>%
#' clinical_significance(
#'   id = subject,
#'   time = time,
#'   outcome = gds,
#'   pre = "pre",
#'   reliability = 0.80
#' )
#'
#' get_method(results)
get_method <- function(x) {
  assert_class(x, "clinisig")

  if (length(x[["method"]]) > 1) {
    x[["method"]][[1]]
  } else {
    x[["method"]]
  }
}
