#' Calc RCI for the HLM method
#'
#' @param data A preprocessed data frame with at least variables `id`, `time`,
#'   and `outcome`
#' @param direction Which direction is beneficial? 1 = higher, -1 = lower
#'
#' @importFrom lme4 lmer ranef
#' @importFrom insight get_parameters get_variance_intercept get_variance_residual get_variance
#' @importFrom dplyr count
#' @importFrom tibble rownames_to_column
#'
#' @return A list with a lmer model, individual coefficients and rci data
#'
#' @noRd
.calc_rci_hlm <- function(data, direction) {
  . <- NULL

  # Fit HLM model
  fitted_model <- data |>
    lmer(outcome ~ time + (time | id), data = _, REML = TRUE)


  # Calculate empirical Bayes estimates, its SD and z (EB / SD). Decide if
  # reliably improved (comparable to RCI calculation in other methods).
  rci_data <- ranef(fitted_model, condVar = TRUE) |>
    as_tibble() |>
    select(id = grp, term, value = condval, sd = condsd) |>
    filter(as.character(term) == "time") |>
    select(id, eb_estimate = value, sd) |>
    mutate(rci = eb_estimate / sd)

  data_with_rci <- .calc_improvement(
      data = rci_data,
      rci_cutoff = 1.96,
      direction = direction
      )

  list(
    model = fitted_model,
    coefficients = rci_data,
    data = data_with_rci
  )
}
