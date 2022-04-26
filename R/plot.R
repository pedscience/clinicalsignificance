#' Plot Clinical Significance Results
#'
#' @param x A clinisig object
#' @param lower_limit Numeric, lower plotting limit. Defaults to 0
#' @param upper_limit Numeric, upper plotting limit. Defaults to 100
#' @param overplotting Numeric, control amount of overplotting. Defaults to 0.02
#'   (i.e., 2% of range between lower and upper limit).
#' @param rci_fill String, a color (name or HEX code) for RCI filling
#' @param rci_alpha Numeric, controls the transparency of the RCI. This can be
#'   any value between 0 and 1.
#' @param diagonal_color String, a color (name or HEX code) for the line
#'   indicating no change.
#' @param show String. You have several options to color different features. Available are
#'  - `"category"` (shows all categories at once) which is the default
#'  - `"recovered"` (shows recovered participants)
#'  - `"improved"` (shows improved participants)
#'  - `"unchanged"` (shows unchanged participants)
#'  - `"deteriorated"` (shows deteriorated participants, if available)
#'  - `"harmed"` (shows harmed participants, if available)
#' @param which String. Which plot type should be shown? Defaults to `"point"`
#'   which yields the default clinical significance plot. The HLM method
#'   incorporates multiple measurements per participant, so a reduction to pre
#'   and post values may remove important information. Therefore, you can
#'   additionally choose to plot either
#'   - each participants trajectory (with `"trajectory"`), or
#'   - each participants fitted Empirical Bayes slope (with `"slope"`)
#' @param include_cutoff Logical. Should the clinical cutoff be plotted as well?
#'   Defaults to `TRUE`.
#' @param include_cutoff_band Logical. If method was HA, a region of uncertainty
#'   around the cutoff can be plotted
#' @param x_lab String, x axis label. Default is `"Pre"` for point and
#'   `"Measurement"` for trajectory and slope plot.
#' @param y_lab String, y axis label. Default is `"Post"` for point, `"Outcome
#'   Score"` for trajectory, and `"Fitted Score"` for slope plot.
#' @param color_lab String, color guide label. Default is `"Group"`.
#' @param ... Additional arguments
#'
#' @import ggplot2
#' @importFrom rlang .data abort
#' @importFrom purrr map2
#' @importFrom tidyr unnest
#'
#' @return A ggplot2 plot
#' @export
plot.clinisig <- function(x,
                          lower_limit = 0,
                          upper_limit = 100,
                          rci_fill = "grey10",
                          rci_alpha = 0.1,
                          diagonal_color = "black",
                          show = NULL,
                          which = c("point", "trajectory", "slope"),
                          include_cutoff = TRUE,
                          include_cutoff_band = FALSE,
                          x_lab = NULL,
                          y_lab = NULL,
                          color_lab = "Group",
                          overplotting = 0.02,
                          ...
                          ) {
  clinisig_method <- get_clinical_significance_method(x)
  which_plot <- arg_match(which)
  cutoff <- get_cutoff(x)[["value"]]
  if (clinisig_method != "HA" & include_cutoff_band) abort("A cutoff band can only be shown for method HA.")


  # Get data for different kind of plots
  if (which_plot == "point") {
    data <- get_augmented_data(x)
  } else if (which_plot == "trajectory") {
    model_data <- get_data(x, "model")
    categories <- get_augmented_data(x)

    if (.has_group(model_data)) join_identifiers <- c("id", "group") else join_identifiers <- c("id")

    data <- model_data %>%
      left_join(categories, by = join_identifiers)
  } else if (which_plot == "slope") {
    min_measurement <- x[["datasets"]][["min"]]
    max_measurement <- x[["datasets"]][["max"]]

    data <- get_augmented_data(x) %>%
      mutate(
        plot_data = map2(.data$intercept, .data$eb_slope, ~ .calc_slope_data(.x, .y, min_measurement, max_measurement))
      ) %>%
      unnest(.data$plot_data)
  }


  # Default plot labels
  if (which_plot == "point" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Pre"
    y_lab <- "Post"
  } else if (which_plot == "trajectory" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Measurement"
    y_lab <- "Outcome Score"
  } else if (which_plot == "slope" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Measurement"
    y_lab <- "Fitted Score"
  }


  # Check if analysis was grouped and display those groups by default
  if (is.null(show) & .has_group(data)) show <- "group"


  # Determine x and y limits for plotting. Overplotting is needed because we
  # want the ribbon to be at the edge of the plot, requiring expand = FALSE in
  # coord_cartesian()
  overplot_amount <- (upper_limit - lower_limit) * overplotting
  lower_limit <- lower_limit - overplot_amount
  upper_limit <- upper_limit + overplot_amount
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
    if (clinisig_method != "HLM") geom_ribbon(data = rci_data, aes(y = NULL, ymin = .data$ymin, ymax = .data$ymax), fill = rci_fill, alpha = rci_alpha),
    geom_abline(color = diagonal_color),
    if (include_cutoff) geom_hline(yintercept = cutoff, lty = 2),
    if (include_cutoff) geom_vline(xintercept = cutoff, lty = 2),
    if (include_cutoff_band) geom_ribbon(data = cs_data, aes(y = NULL, ymin = .data$ymin, ymax = .data$ymax), alpha = rci_alpha),
    if (is.null(show)) geom_point() else geom_point(aes_(color = as.name(show)))
  )

  geom_list_trajectory <- list(
    if (is.null(show)) geom_line(na.rm = TRUE) else geom_line(aes_(color = as.name(show)), na.rm = TRUE)
  )


  # Plot the whole thing
  if (which_plot == "point") {
    data %>%
      ggplot(aes(.data$pre, .data$post)) +
      geom_list +
      coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
      labs(x = x_lab, y = y_lab, color = color_lab) +
      theme_light()
  } else if (which_plot == "trajectory") {
    data %>%
      ggplot(aes(.data$time, .data$outcome, group = .data$id)) +
      geom_list_trajectory +
      labs(x = x_lab, y = y_lab, color = color_lab) +
      theme_light()
  } else if (which_plot == "slope") {
    data %>%
      ggplot(aes(.data$time, .data$fitted, group = .data$id)) +
      geom_list_trajectory +
      labs(x = x_lab, y = y_lab, color = color_lab) +
      theme_light()
  }
}
