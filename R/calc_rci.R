#' Generic to Calculate RCI and Associated Change
#'
#' This is an internal generic and should not be called directly. Depending on
#' the different RCI method requested by the user, the appropriate method is
#' called. It calculates the RCI and according clinical significance category
#' for each participant.
#'
#' @param data Prepped data with class `cs_*`
#' @param ... Additional arguments for the specific RCI method
#'
#' @return RCI result object with class `cs_distribution`
#' @keywords internal
#' @export
calc_rci <- function(data, ...) {
  UseMethod("calc_rci")
}


#' RCI for the Jacobson & Truax method
#'
#' @param direction Beneficial intervention effect for given instrument. 1 =
#'   higher is better, -1 = lower is better
#' @param critical_value Critical RCI value, typically 1.96
#' @param sd_pre Pre measurement SD
#' @param reliability Instrument reliability
#'
#'
#' @keywords internal
#' @export
calc_rci.cs_jt <- function(data,
                           sd_pre,
                           reliability,
                           direction = 1,
                           critical_value = 1.96,
                           ...) {
  data <- data[["data"]]
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)
  s_diff <- .calc_s_diff(se_measurement)


  # Calculate RCI
  rci_data <- data |>
    dplyr::mutate(
      rci = change / s_diff
    )


  # Caluclate categories
  data_with_rci <- .calc_improvement(
    data = rci_data,
    rci_cutoff = critical_value,
    direction = direction
  )

  out <- list(
    s_diff = s_diff,
    data = data_with_rci
  )

  class(out) <- c("cs_distribution", class(out))
  out
}


#' RCI for the Gulliksen, Lord & Novick method
#'
#' @param m_pre Pre measurement mean
#'
#'
#' @keywords internal
#' @export
calc_rci.cs_gln <- function(data,
                            m_pre,
                            sd_pre,
                            reliability,
                            direction = 1,
                            critical_value = 1.96,
                            ...) {
  data <- data[["data"]]
  se_prediction <- .calc_se_prediction(sd_pre = sd_pre, reliability = reliability)

  # Calculate RCI
  rci_data <- data |>
    dplyr::mutate(
      pre_adj = reliability * (pre - m_pre),
      post_adj = post - m_pre,
      change_adj = post_adj - pre_adj,
      rci = change_adj / se_prediction
    )


  # Calculate categories
  data_rci_categories <- .calc_improvement(
    data = rci_data,
    rci_cutoff = critical_value,
    direction = direction
  )

  out <- list(
    se_prediction = se_prediction,
    data = data_rci_categories
  )

  class(out) <- c("cs_distribution", class(out))
  out
}


#' RCI for the Hsu, Lin & Lord method
#'
#' @param m_post Post measurement mean
#'
#'
#' @keywords internal
#' @export
calc_rci.cs_hll <- function(data, m_pre, sd_pre, m_post, reliability, direction = 1, critical_value = 1.96, ...) {
  data <- data[["data"]]
  se_prediction <- .calc_se_prediction(sd_pre = sd_pre, reliability = reliability)


  # Calculate RCI
  rci_data <- data |>
    dplyr::mutate(
      pre_adj = reliability * (pre - m_pre),
      post_adj = post - m_post,
      change_adj = post_adj - pre_adj,
      rci = change_adj / se_prediction
    )


  # Calculate categories
  data_rci_categories <- .calc_improvement(
    data = rci_data,
    rci_cutoff = critical_value,
    direction = direction
  )

  out <- list(
    se_prediction = se_prediction,
    m_post = m_post,
    data = data_rci_categories
  )

  class(out) <- c("cs_distribution", class(out))
  out
}


#' RCI for the Edwards method
#'
#' @keywords internal
#' @export
calc_rci.cs_en <- function(data, m_pre, sd_pre, reliability, direction = 1, critical_value = 1.96, ...) {
  data <- data[["data"]]
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)


  # Calculate the RCI according to reformulation of Speer
  rci_data <- data |>
    dplyr::mutate(
      pre_true = reliability * (pre - m_pre) + m_pre,
      change_adj = post - pre_true,
      rci = change_adj / se_measurement
    )


  # Calculate categories
  data_rci_categories <- .calc_improvement(
    data = rci_data,
    rci_cutoff = critical_value,
    direction = direction
  )

  out <- list(
    se_measurement = se_measurement,
    data = data_rci_categories
  )

  class(out) <- c("cs_distribution", class(out))
  out
}


#' RCI for the NK method
#'
#' @param reliability_post Instrument reliability at post measurement
#'
#' @keywords internal
#' @export
calc_rci.cs_nk <- function(data, m_pre, sd_pre, reliability, reliability_post, direction = 1, critical_value = 1.96, ...) {
  data <- data[["data"]]
  denominator <- sqrt((reliability^2 * sd_pre ^2 * (1 - reliability)) + (sd_pre^2 * (1 - reliability_post)))

  rci_data <- data |>
    dplyr::mutate(
      pre_adj = reliability * (pre - m_pre) + m_pre,
      change_adj = post - pre_adj,
      rci = change_adj / denominator
    )

  data_rci_categories <- .calc_improvement(
    data = rci_data,
    rci_cutoff = critical_value,
    direction = direction
  )

  out <- list(
    reliability_post = reliability_post,
    data = data_rci_categories
  )

  class(out) <- c("cs_distribution", class(out))
  out
}


#' RCI for the Hageman & Arrindell
#'
#' @param sd_post Post measurement SD
#'
#' @keywords internal
#' @export
calc_rci.cs_ha <- function(data, m_pre, sd_pre, m_post, sd_post, reliability, direction = 1, critical_value = 1.65, ...) {
  data <- data[["data"]]
  se_measurement <- .calc_se_measurement(sd_pre = sd_pre, reliability = reliability)
  r_xx_1 <- .calc_reliability_ha(sd = sd_pre, se_measurment = se_measurement)
  r_xx_2 <- .calc_reliability_ha(sd = sd_post, se_measurment = se_measurement)
  cor_pre_post <- stats::cor(data[["pre"]], data[["post"]])

  nominator <- (sd_pre^2 * r_xx_1 + sd_post^2 * r_xx_2 - 2 * sd_pre * sd_post * cor_pre_post)
  denominator <- (sd_pre^2 + sd_post^2 - 2 * sd_pre * sd_post * cor_pre_post)

  r_dd <- nominator / denominator

  rci_data <- data  |>
    dplyr::mutate(
      rci = ((post - pre) * r_dd + (m_post - m_pre) * (1 - r_dd)) / (sqrt(r_dd) * sqrt(2 * se_measurement^2))
    )

  # Caluclate categories
  data_with_rci <- .calc_improvement(
    data = rci_data,
    rci_cutoff = critical_value,
    direction = direction
  )

  out <- list(
    r_dd = r_dd,
    se_measurement = se_measurement,
    data = data_with_rci
  )

  class(out) <- c("cs_distribution", class(out))
  out
}


#' Calc RCI for the HLM method
#'
#' @keywords internal
#' @export
calc_rci.cs_hlm <- function(data, direction, critical_value = 1.96, ...) {
  . <- NULL
  data <- data[["model"]]

  # Fit HLM model
  fitted_model <- data |>
    lme4::lmer(outcome ~ time + (time | id), data = _, REML = TRUE)


  # Calculate empirical Bayes estimates, its SD and z (EB / SD). Decide if
  # reliably improved (comparable to RCI calculation in other methods).
  rci_data <- lme4::ranef(fitted_model, condVar = TRUE) |>
    tibble::as_tibble() |>
    dplyr::select(id = grp, term, value = condval, sd = condsd) |>
    dplyr::filter(as.character(term) == "time") |>
    dplyr::select(id, eb_estimate = value, sd) |>
    dplyr::mutate(rci = eb_estimate / sd)

  data_with_rci <- .calc_improvement(
    data = rci_data,
    rci_cutoff = critical_value,
    direction = direction
  )

  out <- list(
    model = fitted_model,
    coefficients = rci_data,
    data = data_with_rci
  )

  class(out) <- c("cs_distribution", class(out))
  out
}
