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



#' Check if an Object is of Class cs_analysis
#'
#' @param data A data frame
#'
#' @return Boolean
#'
#' @noRd
.check_class <- function(x) {
  if (!inherits(x, "cs_analysis")) cli::cli_abort("The supplied object must be of class {.code cs_analysis} and not {.code {class(x)}}.")
}
