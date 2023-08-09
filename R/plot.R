#' Plot an Object of Class cs_distribution
#'
#' This function creates a generic clinical significance plot bz plotting the
#' patients' pre intervention value on the x-axis and the post intervention
#' score on the y-axis. Additionally, the RCI (region signifying unchanged
#' patients) is shown with a diagonal corresponding to no change.
#'
#' @param x An object of class `cs_distribution`
#' @param x_lab String, x axis label. Default is `"Pre"`.
#' @param y_lab String, x axis label. Default is `"Post"`.
#' @param color_lab String, color label (if colors are displayed). Default is
#'   `"Group"`
#' @param lower_limit Numeric, lower plotting limit. Defaults to 2% smaller
#'   than minimum instrument score
#' @param upper_limit Numeric, upper plotting limit. Defaults to 2% larger than
#'   maximum instrument score
#' @param show Unquoted category name. You have several options to color
#'   different features. Available are
#'   - `category` (shows all categories at once)
#'   - `improved` (shows improved participants)
#'   - `unchanged` (shows unchanged participants)
#'   - `deteriorated` (shows deteriorated participants)
#' @param overplotting Numeric, control amount of overplotting. Defaults to 0.02
#'   (i.e., 2% of range between lower and upper limit).
#' @param rci_fill String, a color (name or HEX code) for RCI fill
#' @param rci_alpha Numeric, controls the transparency of the RCI. This can be
#'   any value between 0 and 1, defaults to 0.1
#' @param ... Additional arguments
#'
#' @return A ggplot2 plot
#' @export
plot.cs_distribution <- function(x,
                                 x_lab = NULL,
                                 y_lab = NULL,
                                 color_lab = "Group",
                                 lower_limit,
                                 upper_limit,
                                 show,
                                 rci_fill = "grey10",
                                 rci_alpha = 0.1,
                                 overplotting = 0.02,
                                 ...) {
  # Which plot should be plotted?
  cs_method <- cs_get_method(x)
  if (cs_method != "HLM") which_plot <- "point" else which_plot <- "trajectory"


  # Get augmented data for plotting
  if (which_plot == "point") {
    data <- cs_get_augmented_data(x) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
      )
  } else if (which_plot == "trajectory") {
    model_data <- cs_get_data(x, "model")
    categories <- cs_get_augmented_data(x)

    if (.has_group(model_data)) by_ids <- dplyr::join_by(c("id", "group")) else by_ids <- dplyr::join_by("id")

    data <- model_data |>
      left_join(categories, by_ids) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
      )
  }




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


  # Generate data for the RCI band
  if (cs_method != "HLM") rci_data <- generate_plotting_band(x, lower_limit = lower_limit, upper_limit = upper_limit)


  # Default plot labels
  if (which_plot == "point" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Pre"
    y_lab <- "Post"
  } else if (which_plot == "trajectory" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Measurement"
    y_lab <- "Outcome Score"
  }


  # Create a list of geoms added to the plot
  if (cs_method != "HLM") {
    geom_list <- list(
      ggplot2::geom_ribbon(data = rci_data, ggplot2::aes(y = NULL, ymin = ymin, ymax = ymax), fill = rci_fill, alpha = rci_alpha),
      ggplot2::geom_abline(color = "grey10"),
      if (.has_group(data) & missing(show)) {
        ggplot2::geom_point(ggplot2::aes(color = group))
      } else {
        ggplot2::geom_point(ggplot2::aes(color = {{ show }}))
      }
    )
  } else if (cs_method == "HLM"){
    geom_list_trajectory <- list(
      if (.has_group(data) & missing(show)) {
        ggplot2::geom_line(ggplot2::aes(color = group), na.rm = TRUE)
      } else {
        ggplot2::geom_line(ggplot2::aes(color = {{ show }}), na.rm = TRUE)
      }
    )
  }


  # Plot the whole thing
  if (which_plot == "point") {
    data |>
      ggplot2::ggplot(ggplot2::aes(pre, post)) +
      geom_list +
      ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
      ggplot2::labs(x = x_lab, y = y_lab, color = color_lab)
  } else if (which_plot == "trajectory") {
    data |>
      ggplot2::ggplot(ggplot2::aes(time, outcome, group = id)) +
      geom_list_trajectory +
      ggplot2::labs(x = x_lab, y = y_lab, color = color_lab) +
      ggplot2::facet_wrap(ggplot2::vars(category))
  }
}




#' Plot an Object of Class cs_statistical
#'
#' @param x An object of class `cs_statistical`
#' @param include_cutoff Logical, whether to include the population cutoff.
#'   Default is `TRUE`.
#' @param show Unquoted category name. You have several options to color
#'   different features. Available are
#'   - `category` (shows all categories at once)
#'   - `clinical_pre` (shows participants with clinical scores pre intervention)
#'   - `functional_post` (shows participants with functional scores post
#'     intervention)
#'   - `unchanged` (shows unchanged participants)
#'
#' @inheritParams plot.cs_distribution
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' claus_results <- claus_2020 |>
#'   cs_statistical(id, time, bdi, m_functional = 8, sd_functional = 7, pre = 1, post = 4, cutoff_type = "c")
#'
#' plot(claus_results)
#' plot(claus_results, show = category)
#'
#'
#' claus_results_ha <- claus_2020 |>
#'   cs_statistical(id, time, bdi, m_functional = 8, sd_functional = 7, pre = 1, post = 4, reliability = 0.80, cutoff_type = "c", cutoff_method = "HA")
#'
#' plot(claus_results_ha)
#' plot(claus_results_ha, show = category)
plot.cs_statistical <- function(x,
                                x_lab = "Pre",
                                y_lab = "Post",
                                color_lab = "Group",
                                include_cutoff = TRUE,
                                lower_limit,
                                upper_limit,
                                show,
                                overplotting = 0.02,
                                ...) {
  cs_method <- cs_get_method(x)


  # Get data
  data <- cs_get_augmented_data(x) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
    )

  # Get the cutoff
  cs_cutoff <- cs_get_cutoff(x)[["value"]]


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


  # Create a list of geoms that can be added to the plot
  geom_list <- list(
    ggplot2::geom_vline(xintercept = cs_cutoff, lty = "dashed"),
    ggplot2::geom_hline(yintercept = cs_cutoff, lty = "dashed"),
    ggplot2::geom_abline(color = "grey10"),
    if (.has_group(data) & missing(show)) {
      ggplot2::geom_point(ggplot2::aes(color = group))
    } else {
      ggplot2::geom_point(ggplot2::aes(color = {{ show }}))
    }
  )


  # Plot the whole thing
  data |>
    ggplot2::ggplot(ggplot2::aes(pre, post)) +
    geom_list +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    ggplot2::labs(x = x_lab, y = y_lab, color = color_lab)
}



#' Plot an Object of Class cs_combined
#'
#' This function creates a generic clinical significance plot bz plotting the
#' patients' pre intervention value on the x-axis and the post intervention
#' score on the y-axis. Additionally, the RCI (region signifying unchanged
#' patients) is shown with a diagonal corresponding to no change.
#'
#' @param x An object of class `cs_distribution`
#' @param x_lab String, x axis label. Default is `"Pre"`.
#' @param y_lab String, x axis label. Default is `"Post"`.
#' @param color_lab String, color label (if colors are displayed). Default is
#'   `"Group"`
#' @param lower_limit Numeric, lower plotting limit. Defaults to 2% smaller
#'   than minimum instrument score
#' @param upper_limit Numeric, upper plotting limit. Defaults to 2% larger than
#'   maximum instrument score
#' @param show Unquoted category name. You have several options to color
#'   different features. Available are
#'   - `category` (shows all categories at once)
#'   - `recovered` (shows recovered participants)
#'   - `improved` (shows improved participants)
#'   - `unchanged` (shows unchanged participants)
#'   - `deteriorated` (shows deteriorated participants)
#'   - `harmed` (shows harmed participants)
#' @param overplotting Numeric, control amount of overplotting. Defaults to 0.02
#'   (i.e., 2% of range between lower and upper limit).
#' @param rci_fill String, a color (name or HEX code) for RCI fill
#' @param rci_alpha Numeric, controls the transparency of the RCI. This can be
#'   any value between 0 and 1, defaults to 0.1
#' @param ... Additional arguments
#'
#' @return A ggplot2 plot
#' @export
plot.cs_combined <- function(x,
                             x_lab = NULL,
                             y_lab = NULL,
                             color_lab = "Group",
                             lower_limit,
                             upper_limit,
                             show,
                             rci_fill = "grey10",
                             rci_alpha = 0.1,
                             overplotting = 0.02,
                             ...) {
  # Which plot should be plotted?
  cs_method <- cs_get_method(x)
  if (cs_method != "HLM") which_plot <- "point" else which_plot <- "trajectory"


  # Get augmented data for plotting
  if (which_plot == "point") {
    data <- cs_get_augmented_data(x) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
      )
  } else if (which_plot == "trajectory") {
    model_data <- cs_get_data(x, "model")
    categories <- cs_get_augmented_data(x)


    # Join the data accordingly
    if (.has_group(model_data)) by_ids <- dplyr::join_by(c("id", "group")) else by_ids <- dplyr::join_by("id")

    data <- model_data |>
      dplyr::left_join(categories, by_ids) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
      )
  }


  # Get the cutoff
  cs_cutoff <- cs_get_cutoff(x)[["value"]]


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


  # Generate data for the RCI band
  if (cs_method != "HLM") rci_data <- generate_plotting_band(x, lower_limit = lower_limit, upper_limit = upper_limit)


  # Default plot labels
  if (which_plot == "point" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Pre"
    y_lab <- "Post"
  } else if (which_plot == "trajectory" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Measurement"
    y_lab <- "Outcome Score"
  }


  # Create a list of geoms added to the plot
  if (cs_method != "HLM") {
    geom_list <- list(
      ggplot2::geom_vline(xintercept = cs_cutoff, lty = "dashed"),
      ggplot2::geom_hline(yintercept = cs_cutoff, lty = "dashed"),
      ggplot2::geom_ribbon(data = rci_data, ggplot2::aes(y = NULL, ymin = ymin, ymax = ymax), fill = rci_fill, alpha = rci_alpha),
      ggplot2::geom_abline(color = "grey10"),
      if (.has_group(data) & missing(show)) {
        ggplot2::geom_point(ggplot2::aes(color = group))
      } else {
        ggplot2::geom_point(ggplot2::aes(color = {{ show }}))
      }
    )
  } else if (cs_method == "HLM"){
    geom_list_trajectory <- list(
      if (.has_group(data) & missing(show)) {
        ggplot2::geom_line(ggplot2::aes(color = group), na.rm = TRUE)
      } else {
        ggplot2::geom_line(ggplot2::aes(color = {{ show }}), na.rm = TRUE)
      }
    )
  }


  # Plot the whole thing
  if (which_plot == "point") {
    data |>
      ggplot2::ggplot(ggplot2::aes(pre, post)) +
      geom_list +
      ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
      ggplot2::labs(x = x_lab, y = y_lab, color = color_lab)
  } else if (which_plot == "trajectory") {
    data |>
      ggplot2::ggplot(ggplot2::aes(time, outcome, group = id)) +
      geom_list_trajectory +
      ggplot2::labs(x = x_lab, y = y_lab, color = color_lab) +
      ggplot2::facet_wrap(ggplot2::vars(category))
  }
}




#' Plot an Object of Class cs_percentage
#'
#' This function creates a generic clinical significance plot bz plotting the
#' patients' pre intervention value on the x-axis and the post intervention
#' score on the y-axis. Additionally, the RCI (region signifying unchanged
#' patients) is shown with a diagonal corresponding to no change.
#'
#' @param x An object of class `cs_percentage`
#' @param x_lab String, x axis label. Default is `"Pre"`.
#' @param y_lab String, x axis label. Default is `"Post"`.
#' @param color_lab String, color label (if colors are displayed). Default is
#'   `"Group"`
#' @param lower_limit Numeric, lower plotting limit. Defaults to 2% smaller
#'   than minimum instrument score
#' @param upper_limit Numeric, upper plotting limit. Defaults to 2% larger than
#'   maximum instrument score
#' @param show Unquoted category name. You have several options to color
#'   different features. Available are
#'   - `category` (shows all categories at once)
#'   - `improved` (shows improved participants)
#'   - `unchanged` (shows unchanged participants)
#'   - `deteriorated` (shows deteriorated participants)
#' @param overplotting Numeric, control amount of overplotting. Defaults to 0.02
#'   (i.e., 2% of range between lower and upper limit).
#' @param rci_fill String, a color (name or HEX code) for RCI fill
#' @param rci_alpha Numeric, controls the transparency of the RCI. This can be
#'   any value between 0 and 1, defaults to 0.1
#' @param ... Additional arguments
#'
#' @return A ggplot2 plot
#' @export
plot.cs_percentage<- function(x,
                              x_lab = "Pre",
                              y_lab = "Post",
                              color_lab = "Group",
                              lower_limit,
                              upper_limit,
                              show,
                              rci_fill = "grey10",
                              rci_alpha = 0.1,
                              overplotting = 0.02,
                              ...) {
  # Get augmented data for plotting
  data <- cs_get_augmented_data(x) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
    )


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


  # Generate data for the RCI band
  band_data <- generate_plotting_band(x, lower_limit = lower_limit, upper_limit = upper_limit)


  # Create a list of geoms added to the plot
    geom_list <- list(
      ggplot2::geom_ribbon(data = band_data, ggplot2::aes(y = NULL, ymin = ymin, ymax = ymax), fill = rci_fill, alpha = rci_alpha),
      ggplot2::geom_abline(color = "grey10"),
      if (.has_group(data) & missing(show)) {
        ggplot2::geom_point(ggplot2::aes(color = group))
      } else {
        ggplot2::geom_point(ggplot2::aes(color = {{ show }}))
      }
    )


  # Plot the whole thing
    data |>
      ggplot2::ggplot(ggplot2::aes(pre, post)) +
      geom_list +
      ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
      ggplot2::labs(x = x_lab, y = y_lab, color = color_lab)
}
