#' Calculate Change for the Percentage-Change Approach
#'
#' This is an internal generic and should not be called directly. Depending on
#' the different percent change cutoff requested by the user, this function
#' calculates the relative change according clinical significance category for
#' each patient.
#'
#' @inheritParams calc_rci
#' @param data A pre-processed data frame with at least columns `change` and
#'   `pre`
#' @param pct_improvement Numeric, the required percent change cutoff to infer
#'   an improvement
#' @param pct_deterioration Numeric, the required percent change cutoff to infer
#'   a deterioration
#'
#' @return An object of classm `cs_percentage` list with participant-wise info
#'   on clinical significant change catergory
#'
#' @keywords internal
#' @export
calc_percentage <- function(data, pct_improvement, pct_deterioration, direction) {
  out <- data |>
    dplyr::mutate(
      pct_change   = change / pre,
      improved     = (direction * pct_change >= pct_improvement) | (pre == 0 & post > 0),
      improved     = ifelse(pre == post, FALSE, improved),
      deteriorated = direction * pct_change <= -pct_deterioration | (pre == 0 & post < 0),
      deteriorated = ifelse(pre == post, FALSE, deteriorated),
      unchanged = (!improved & !deteriorated) | (pre == post)
    ) |>
    dplyr::select(id, pct_change:unchanged)

  class(out) <- c("cs_percentage", class(out))
  out
}
