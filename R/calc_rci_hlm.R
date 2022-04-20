.calc_rci_hlm <- function(data, direction) {
  # Fit HLM model
  fitted_model <- data %>%
    lmer(outcome ~ time + (time | id), data = ., REML = TRUE)


  # Extract needed components
  gamma_00 <- get_parameters(fitted_model, "fixed")[["Estimate"]][[2]]
  tau_00 <- get_variance_intercept(fitted_model)
  sigma_squared <- get_variance_residual(fitted_model)
  var_gamma_00 <- get_variance(fitted_model)[["var.slope"]]


  # Get n observations per individual. Calculate lambdas (reliability of the
  # mean of individual j)
  lambdas <- data %>%
    na.omit() %>%
    count(id) %>%
    mutate(
      v_j = sigma_squared / n,
      lambda_j = tau_00 / (tau_00 + v_j)
    )


  # Calculate empirical Bayes estimates, its SD and z (EB / SD). Decide if
  # reliably improved (comparable to RCI calculation in other methods).
  rci_data <- coef(fitted_model)[["id"]] %>%
    rename(intercept = 1, slope = 2) %>%
    rownames_to_column("id") %>%
    as_tibble() %>%
    left_join(lambdas, by = "id") %>%
    mutate(
      eb_slope = lambda_j * slope + (1 - lambda_j) * gamma_00,
      var_eb_slope = (v_j^-1 + tau_00^-1)^-1 + (1 - lambda_j^2) * var_gamma_00,
      sd_eb_slope = sqrt(var_eb_slope),
      z = eb_slope / sd_eb_slope
    ) %>%
    select(id, n, intercept, slope, eb_slope, sd_eb_slope, rci = z)

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
