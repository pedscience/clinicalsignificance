#' Check the Clinical Significance Cutoff Based on Population Descriptives
#'
#' @param object An object of class clinisig. This is optional and can be used
#'   to visualize cutoffs after conducting a clinical significance analysis.
#' @param m_clinical Mean of clinical population
#' @param sd_clinical SD of clinical population
#' @param m_functional M of functional population
#' @param sd_functional SD of functional population
#' @param resolution Curve resolution (number of points to be drawn). This may
#'   improve smoothness of highly peaked curves.
#' @inheritParams clinical_significance
#'
#' @return A ggplot2
#' @export
check_cutoff <- function(object = NULL, m_clinical, sd_clinical, m_functional = NA, sd_functional = NA, type = c("a", "b", "c"), better_is = c("lower", "higher"), resolution = 300) {
  dnorm <- NULL
  # Get effect direction and cutoff type to show
  if (arg_match(better_is) == "lower") direction <- -1 else direction <- 1
  type <- arg_match(type)
  if (!missing(m_functional) & !missing(sd_functional)) has_functional <- TRUE else has_functional <- FALSE


  # Extract descriptives if a clinisig object is specified
  if (class(object) == "clinisig") {
    cutoff <- object$cutoff$info
    direction <- object$cutoff$direction
    type <- cutoff$type

    if (type == "HA") abort("Currently, cutoffs of type HA are not supported.")

    m_clinical <- cutoff$m_clinical
    sd_clinical <- cutoff$sd_clinical
    m_functional <- cutoff$m_functional
    sd_functional <- cutoff$sd_functional
  } else if (!is.null(object) & class(object) != "clinisig") {
    abort("The object you specified is not a clinisig object.")
  }


  # Plotting limits
  clinical_min <- m_clinical - 4 * sd_clinical
  functional_min <- m_functional - 4 * sd_functional
  lower_limit <- min(clinical_min, functional_min, na.rm = TRUE)

  clinical_max <- m_clinical + 4 * sd_clinical
  functional_max <- m_functional + 4 * sd_functional
  upper_limit <- max(clinical_max, functional_max, na.rm = TRUE)


  # Linetypes
  if (type == "a") lty_a <- 1 else lty_a <- 2
  if (type == "b") lty_b <- 1 else lty_b <- 2
  if (type == "c") lty_c <- 1 else lty_c <- 2


  # Calculation of cutoffs
  cut_a <- .calc_cutoff_jt(m_clinical, sd_clinical, m_functional, sd_functional, type = "a", direction = direction)[["value"]]
  if (has_functional) {
    cut_b <- .calc_cutoff_jt(m_clinical, sd_clinical, m_functional, sd_functional, type = "b", direction = direction)[["value"]]
    cut_c <- .calc_cutoff_jt(m_clinical, sd_clinical, m_functional, sd_functional, type = "c", direction = direction)[["value"]]
  }


  # All geoms
  geom_list <- list(
    geom_function(aes(color = "Clinical"), fun = dnorm, args = list(mean = m_clinical, sd = sd_clinical), n = resolution),
    if (has_functional) geom_function(aes(color = "Functional"), fun = dnorm, args = list(mean = m_functional, sd = sd_functional), n = resolution),
    geom_vline(xintercept = cut_a, lty = lty_a),
    if (has_functional) geom_vline(xintercept = cut_b, lty = lty_b),
    if (has_functional) geom_vline(xintercept = cut_c, lty = lty_c),
    geom_label(aes(label = "a"), x = cut_a, y = 0.005, label.r = unit(0, "lines")),
    if (has_functional) geom_label(aes(label = "b"), x = cut_b, y = 0.005, label.r = unit(0, "lines")),
    if (has_functional) geom_label(aes(label = "c"), x = cut_c, y = 0.005, label.r = unit(0, "lines"))
  )


  # Put it all together
  ggplot() +
    geom_list +
    expand_limits(x = c(lower_limit, upper_limit), y = 0) +
    theme_light() +
    labs(x = "Instrument Score", y = "Density", color = "Population")
}
