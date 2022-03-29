#' Get A Summary Table From A clinisig Object
#'
#' @inheritParams get_data
#'
#' @return A tibble with clinical significance categories
#' @export
get_summary <- function(x) {
  x[["summary"]]
}
