calc_percentage <- function(data, pct_improvement, pct_deterioration, direction) {
  out <- data |>
    dplyr::mutate(
      pct_change   = change / pre,
      improved     = (direction * pct_change >= pct_improvement) | (pre == 0 & post > 0),
      improved     = ifelse(pre == post, FALSE, improved),
      deteriorated = direction * pct_change <= -pct_deterioration,
      deteriorated = ifelse(pre == post, FALSE, deteriorated),
      unchanged = (!improved & !deteriorated) | (pre == post)
    ) |>
    dplyr::select(id, pct_change:unchanged)

  class(out) <- c("cs_percentage", class(out))
  out
}
