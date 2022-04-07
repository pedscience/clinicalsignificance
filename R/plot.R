#' Plot Clinical Significance Results
#'
#' @param x A clinisig object
#' @param lower_limit Numeric, lower plotting limit. Defaults to 0
#' @param upper_limit Numeric, upper plotting limit. Defaults to 100
#' @param limit_tolerance Numeric, control amount of overplotting. Defaults to
#'   0.02 (i.e., 2% of range between lower and upper limit).
#' @param rci_fill String, a color (name or HEX code) for RCI filling
#' @param rci_alpha Numeric, controls the transparency of the RCI. This can be
#'   any value between 0 and 1.
#' @param ab_line_color String, a color (name or HEX code) for the line
#'   indicating no change.
#' @param show String, which category should be shown with distinctive colors?
#'   Available are `"recovered"`, `"improved"`, `"deteriorated"`, or
#'   `"unchanged"`.
#' @param include_cutoff Logical, should the clinical cutoff be plotted as well?
#'   Defaults to `TRUE`.
#' @param include_cutoff_band If method was HA, a region of uncertainty around
#'   the cutoff can be plotted
#' @param x_lab String, x axis label. Default is `"Pre"`.
#' @param y_lab String, y axis label. Default is `"Post"`.
#' @param ... Additional arguments
#'
#' @import ggplot2
#' @importFrom rlang .data abort
#'
#' @return A ggplot2 plot
#' @export
plot.clinisig <- function(x,
                          lower_limit = 0,
                          upper_limit = 100,
                          rci_fill = "grey10",
                          rci_alpha = 0.1,
                          ab_line_color = "black",
                          show = NULL,
                          include_cutoff = TRUE,
                          include_cutoff_band = FALSE,
                          x_lab = "Pre",
                          y_lab = "Post",
                          limit_tolerance = 0.02, ...
                          ) {
  clinisig_method <- get_clinical_significance_method(x)
  data <- get_augmented_data(x)
  cutoff <- get_cutoff(x)[["value"]]
  if (is.null(show) & .has_group(data)) show <- "group"

  if (clinisig_method != "HA" & include_cutoff_band) abort("A cutoff band can only be shown for method HA.")
  if (!(clinisig_method %in% c("JT", "EN", "GLN", "HA", "HLL", "NK"))) abort(paste0("Currently, there is no print method implemented for clinical significance method ", clinisig_method))

  # Determine x and y limits for plotting. Overplotting is needed because we
  # want the ribbon to be at the edge of the plot, requiring expand = FALSE in
  # coord_cartesian()
  overplotting <- (upper_limit - lower_limit) * limit_tolerance
  lower_limit <- lower_limit - overplotting
  upper_limit <- upper_limit + overplotting
  x_limits <- y_limits <- c(lower_limit, upper_limit)


  # Generate RCI data for ribbon
  if (clinisig_method == "JT") {
    rci_data <- .generate_rci_data_jt(
      x = x,
      lower_limit = lower_limit,
      upper_limit = upper_limit
    )
  } else if (clinisig_method == "GLN") {
    rci_data <- .generate_rci_data_gln(
      x = x, lower_limit = lower_limit,
      upper_limit = upper_limit
    )
  } else if (clinisig_method == "HLL") {
    rci_data <- .generate_rci_data_hll(
      x = x,
      lower_limit = lower_limit,
      upper_limit = upper_limit
    )
  } else if (clinisig_method == "EN") {
    rci_data <- .generate_rci_data_en(
      x = x,
      lower_limit = lower_limit,
      upper_limit = upper_limit
    )
  } else if (clinisig_method == "NK") {
    rci_data <- .generate_rci_data_nk(
      x = x,
      lower_limit = lower_limit,
      upper_limit = upper_limit
    )
  } else if (clinisig_method == "HA") {
    rci_data <- .generate_rci_data_ha(
      x = x,
      lower_limit = lower_limit,
      upper_limit = upper_limit
    )

    cs_data <- .generate_true_cut_data(
      x = x,
      lower_limit = lower_limit,
      upper_limit = upper_limit
    )
  }


  # Create a list of geoms added to the plot
  geom_list <- list(
    geom_ribbon(data = rci_data, aes(y = NULL, ymin = .data$ymin, ymax = .data$ymax), fill = rci_fill, alpha = rci_alpha),
    geom_abline(color = ab_line_color),
    if (include_cutoff) geom_hline(yintercept = cutoff, lty = 2),
    if (include_cutoff) geom_vline(xintercept = cutoff, lty = 2),
    if (include_cutoff_band) geom_ribbon(data = cs_data, aes(y = NULL, ymin = .data$ymin, ymax = .data$ymax), alpha = rci_alpha),
    if (is.null(show)) geom_point() else geom_point(aes_(color = as.name(show)))
  )


  # Plot the whole thing
  data %>%
    ggplot(aes(.data$pre, .data$post)) +
    geom_list +
    coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    labs(x = x_lab, y = y_lab) +
    theme_light()
}
