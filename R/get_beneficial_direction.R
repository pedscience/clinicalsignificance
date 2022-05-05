#' Get the Specified Beneficial Direction of a Clinical Significance Analysis
#'
#' @param x A clinisig object
#' @param ... Additional arguments
#'
#' @return A string
#' @export
get_beneficial_direction <- function(x, ...) {
  assert_class(x, "clinisig")

  direction <- x[["cutoff"]][["direction"]]

  ifelse(direction == 1, "higher", "lower")
}
