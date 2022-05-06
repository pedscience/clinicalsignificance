#' Get the Specified Beneficial Direction of a Clinical Significance Analysis
#'
#' @param x A clinisig object
#' @param ... Additional arguments
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
#' get_beneficial_direction(results)

get_beneficial_direction <- function(x, ...) {
  assert_class(x, "clinisig")

  direction <- x[["cutoff"]][["direction"]]

  ifelse(direction == 1, "higher", "lower")
}
