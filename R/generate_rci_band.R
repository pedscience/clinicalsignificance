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
generate_rci_band <- function(x, lower_limit, upper_limit, ...) {
  UseMethod("generate_rci_band")
}


#' Generate RCI Band for JT Method
#'
#' @inheritParams generate_rci_band
#'
#' @return A tibble
#' @export
generate_rci_band.cs_jt <- function(x, lower_limit = 0, upper_limit = 100, ...) {
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
#' @inheritParams generate_rci_band
#'
#' @return A tibble
#' @export
generate_rci_band.cs_gln <- function(x, lower_limit = 0, upper_limit = 100, ...) {
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
#' @inheritParams generate_rci_band
#'
#' @return A tibble
#' @export
generate_rci_band.cs_hll <- function(x, lower_limit = 0, upper_limit = 100, ...) {
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
