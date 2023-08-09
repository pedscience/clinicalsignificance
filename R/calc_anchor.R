#' Generic to Calculate Anchor-Based Results
#'
#' @param data A datasets object
#' @param mid_improvement Numeric, change that indicates a clinically
#'   significant improvement
#' @param mid_deterioration Numeric, change that indicates a clinically
#'   significant deterioration
#' @param direction Which direction is beneficial? Lower = -1, better = 1
#'
#' @return An object of class `cs_anchor`
#' @export
calc_anchor <- function(data,
                        mid_improvement,
                        mid_deterioration,
                        direction) {
  UseMethod("calc_anchor")
}



#' Anchor Calculations for Individual Results
#'
#' This is an internal function and should never be called directly.
#'
#' @inheritParams calc_anchor
#'
#' @return An object of class `cs_anchor_individual`
#' @export
calc_anchor.cs_anchor_individual <- function(data, mid_improvement, mid_deterioration, direction) {
  out <- data[["data"]] |>
    dplyr::mutate(
      improved     = direction * change >= mid_improvement,
      deteriorated = direction * change <= -mid_deterioration,
      unchanged = !improved & !deteriorated
    ) |>
    dplyr::select(id, improved:unchanged)

  class(out) <- c("cs_anchor_individual", class(out))
  out
}
