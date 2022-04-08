get_beneficial_direction <- function(x, ...) {
  direction <- x[["cutoff"]][["direction"]]

  ifelse(direction == 1, "higher", "lower")
}
