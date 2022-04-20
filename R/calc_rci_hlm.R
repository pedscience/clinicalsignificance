#' Calc RCI for the HLM method
#'
#' @param data A preprocessed data frame with at least variables `id`, `time`,
#'   and `outcome`
#' @param direction Which direction is beneficial? 1 = higher, -1 = lower
#'
#' @importFrom lme4 lmer
#' @importFrom insight get_parameters get_variance_intercept get_variance_residual get_variance
#' @importFrom dplyr count
#' @importFrom rlang .data
#' @importFrom stats coef
#' @importFrom tibble rownames_to_column
#'
#' @return A list with a lmer model, individual coefficients and rci data
#'
#' @noRd
.calc_rci_hlm <- function(data, direction) {
  # Fit HLM model
  fitted_model <- data %>%
    lmer(outcome ~ time + (time | id), data = .data$., REML = TRUE)


  # Extract needed components
  gamma_00 <- get_parameters(fitted_model, "fixed")[["Estimate"]][[2]]
  tau_00 <- get_variance_intercept(fitted_model)
  sigma_squared <- get_variance_residual(fitted_model)
  var_gamma_00 <- get_variance(fitted_model)[["var.slope"]]


  # Get n observations per individual. Calculate lambdas (reliability of the
  # mean of individual j)
  lambdas <- data %>%
    na.omit() %>%
    count(.data$id) %>%
    mutate(
      v_j = sigma_squared / .data$n,
      lambda_j = tau_00 / (tau_00 + .data$v_j)
    )


  # Calculate empirical Bayes estimates, its SD and z (EB / SD). Decide if
  # reliably improved (comparable to RCI calculation in other methods).
  rci_data <- coef(fitted_model)[["id"]] %>%
    rename(intercept = 1, slope = 2) %>%
    rownames_to_column("id") %>%
    as_tibble() %>%
    left_join(lambdas, by = "id") %>%
    mutate(
      eb_slope = .data$lambda_j * .data$slope + (1 - .data$lambda_j) * gamma_00,
      var_eb_slope = (.data$v_j^-1 + tau_00^-1)^-1 + (1 - .data$lambda_j^2) * var_gamma_00,
      sd_eb_slope = sqrt(.data$var_eb_slope),
      z = .data$eb_slope / .data$sd_eb_slope
    ) %>%
    select(.data$id, .data$n, .data$intercept, .data$slope, .data$eb_slope, .data$sd_eb_slope, rci = .data$z)

  data_with_rci <- rci_data %>%
    .calc_improvement(
      rci_cutoff = 1.96,
      direction = direction
      )


  list(
    model = fitted_model,
    coefficients = rci_data,
    data = data_with_rci
  )
}
