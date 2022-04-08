#' Get Descriptives Used In The Cutoff Calculation
#'
#' @param x A clinisg object
#'
#' @importFrom rlang .data
#'
#' @return A tibble with means and standard deviations of the clinical and
#'   functional population
#' @export
get_cutoff_descriptives <- function(x) {
  x[["cutoff"]][["info"]] %>%
    as_tibble() %>%
    select(-.data$type, -.data$value)
}
