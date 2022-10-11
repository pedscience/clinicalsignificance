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
  . <- NULL

  # Fit HLM model
  fitted_model <- data %>%
    lmer(outcome ~ time + (time | id), data = ., REML = TRUE)


  # Calculate empirical Bayes estimates, its SD and z (EB / SD). Decide if
  # reliably improved (comparable to RCI calculation in other methods).
  rci_data <- ranef(fitted_model, condVar = TRUE) %>%
    as_tibble() %>%
    select(id = grp, term, value = condval, sd = condsd) %>%
    mutate(
      term = snakecase::to_snake_case(as.character(term))
    ) %>%
    pivot_wider(names_from = term, values_from = c(value, sd), id_cols = id) %>%
    select(id, intercept = value_intercept, eb_slope = value_time, sd_eb_slope = sd_time) %>%
    mutate(
      rci = eb_slope / sd_eb_slope
    ) %>%
    select(.data$id, .data$intercept, .data$eb_slope, .data$sd_eb_slope, .data$rci)

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
