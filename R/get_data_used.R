#' Retrieve used data from a clinisig object
#'
#' @param x A clinisig object
#' @param ... Additional arguments
#'
#' @return A tibble with data used in calculations
#' @export
get_data_used <- function(x, ...) {
  x[["datasets"]][["data"]]
}
