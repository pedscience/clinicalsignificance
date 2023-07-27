#' Generic to Calculate RCI Band Data for Plotting
#'
#' This is an internal generic to generate data for the RCI band in clinical
#' significance plots. This should not be called directly.
#'
#' @param x An object of respective class
#' @param lower_limit Lower plotting limit
#' @param upper_limit Upper plotting limit
#' @param ... Additional arguments
#'
#' @return A tibble
#' @export
generate_plotting_band <- function(x, lower_limit, upper_limit, ...) {
  UseMethod("generate_plotting_band")
}


#' Generate RCI Band for JT Method
#'
#' @inheritParams generate_plotting_band
#'
#' @return A tibble
#' @export
generate_plotting_band.cs_jt <- function(x, lower_limit = 0, upper_limit = 100, ...) {
  s_diff <- x[["rci_results"]][[1]]
  critical_value <- x[["critical_value"]]

  tibble::tibble(
    pre = c(lower_limit, upper_limit),
    ymin = pre - critical_value * s_diff,
    ymax = pre + critical_value * s_diff
  )
}


#' Generate RCI Band for GLN Method
#'
#' @inheritParams generate_plotting_band
#'
#' @return A tibble
#' @export
generate_plotting_band.cs_gln <- function(x, lower_limit = 0, upper_limit = 100, ...) {
  s_prediction <- x[["rci_results"]][[1]]
  reliability <- cs_get_reliability(x)[[1]]
  m_pre <- mean(cs_get_data(x)[["pre"]])
  critical_value <- x[["critical_value"]]

  tibble(
    pre = c(lower_limit, upper_limit),
    ymin = reliability * pre - reliability * m_pre + m_pre + (-critical_value * s_prediction),
    ymax = reliability * pre - reliability * m_pre + m_pre + (critical_value * s_prediction)
  )
}


#' Generate RCI Band for HLL Method
#'
#' @inheritParams generate_plotting_band
#'
#' @return A tibble
#' @export
generate_plotting_band.cs_hll <- function(x, lower_limit = 0, upper_limit = 100, ...) {
  s_prediction <- x[["rci_results"]][[1]]
  m_post <- x[["rci_results"]][["m_post"]]
  reliability <- cs_get_reliability(x)[[1]]
  m_pre <- mean(cs_get_data(x)[["pre"]])
  critical_value <- x[["critical_value"]]

  tibble(
    pre = c(lower_limit, upper_limit),
    ymin = -critical_value * s_prediction + m_post + reliability * pre - reliability * m_pre,
    ymax = critical_value * s_prediction + m_post + reliability * pre - reliability * m_pre
  )
}


#' Generate RCI Band for EN Method
#'
#' @inheritParams generate_plotting_band
#'
#' @return A tibble
#' @export
generate_plotting_band.cs_en <- function(x, lower_limit = 0, upper_limit = 100, ...) {
  se_measurement <- x[["rci_results"]][[1]]
  reliability <- cs_get_reliability(x)[[1]]
  m_pre <- mean(cs_get_data(x)[["pre"]])
  critical_value <- x[["critical_value"]]

  tibble(
    pre = c(lower_limit, upper_limit),
    pre_true = reliability * (pre - m_pre) + m_pre,
    ymin = pre_true - critical_value * se_measurement,
    ymax = pre_true + critical_value * se_measurement
  )
}


#' Generate RCI Band for NK Method
#'
#' @inheritParams generate_plotting_band
#'
#' @return A tibble
#' @export
generate_plotting_band.cs_nk <- function(x, lower_limit = 0, upper_limit = 100, ...) {
  m_pre <- mean(cs_get_data(x)[["pre"]])
  sd_pre <- stats::sd(cs_get_data(x)[["pre"]])
  reliability_pre <- cs_get_reliability(x)[[1]]
  reliability_post <- cs_get_reliability(x)[[2]]
  critical_value <- x[["critical_value"]]

  tibble(
    pre = c(lower_limit, upper_limit),
    ymin = -critical_value * sqrt((reliability_pre^2 * sd_pre^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post))) + (reliability_pre * (pre - m_pre) + m_pre),
    ymax = critical_value * sqrt((reliability_pre^2 * sd_pre^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post))) + (reliability_pre * (pre - m_pre) + m_pre)
  )
}


#' Generate RCI Band for HA Method
#'
#' @inheritParams generate_plotting_band
#'
#' @return A tibble
#' @export
generate_plotting_band.cs_ha <- function(x, lower_limit = 0, upper_limit = 100, ...) {
  r_dd <- x[["rci_results"]][[1]]
  se_measurement <- x[["rci_results"]][[2]]
  m_pre <- mean(cs_get_data(x)[["pre"]])
  m_post <- mean(cs_get_data(x)[["post"]])
  critical_value <- x[["critical_value"]]

  tibble(
    pre = c(lower_limit, upper_limit),
    ymin = (-critical_value * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd,
    ymax = (critical_value * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd
  )
}