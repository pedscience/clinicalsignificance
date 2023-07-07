#' Plot an Object of Class cs_distribution
#'
#' @param x
#' @param ...
#' @param x_lab
#' @param y_lab
#'
#' @return
#' @export
#'
#' @examples
plot.cs_distribution <- function(x, x_lab = "Pre", y_lab = "Post", lower_limit, upper_limit, overplotting = 0.02, ...) {
  cs_method <- cs_get_method(x)
  data <- cs_get_augmented_data(x)


  # If lower and upper limit are not supplied, get them based on the data
  if (missing(lower_limit)) lower_limit <- min(data[["pre"]], data[["post"]])
  if (missing(upper_limit)) upper_limit <- max(data[["pre"]], data[["post"]])

  # Determine x and y limits for plotting. Overplotting is needed because we
  # want the ribbon to be at the edge of the plot, thus requiring expand = FALSE
  # in coord_cartesian()
  overplot_amount <- (upper_limit - lower_limit) * overplotting
  lower_limit <- lower_limit - overplot_amount
  upper_limit <- upper_limit + overplot_amount
  x_limits <- y_limits <- c(lower_limit, upper_limit)

  list(
    method = cs_method,
    data = data,
    labels = c(x_lab, y_lab),
    limits = x_limits
  )
}
