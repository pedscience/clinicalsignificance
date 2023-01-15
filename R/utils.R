#' Check if a data frame has a column named group
#'
#' @param data A data frame
#'
#' @return Boolean
#'
#' @noRd
.has_group <- function(data) {
  "group" %in% names(data)
}
