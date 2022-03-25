#' Clinical Significance
#'
#' @param data A tidy dataframe
#' @param id Participant ID
#' @param time Time variable
#' @param outcome Outcome variable
#' @param measurements If `time` contains more than two values, you can specify
#'   your two measurements of interest as a vector
#' @param baseline If your `time` is of type character, you can specify the pre
#'   measurement
#' @param m_functional Mean of the functional population
#' @param sd_functional Standard deviation of the functional population
#' @param type Cutoff type. Available are `"a"`, `"b"`, and `"c"`. Defaults to
#'   `"a"` (dee details for further information in which cutoff to choose).
#' @param reliability The instrument's reliability estimate.
#' @param better_is Which direction means a better outcome? Available are
#'   `"lower"` and `"higher"`. Defaults to `"higher"`.
#'
#' @importFrom dplyr relocate bind_cols
#'
#' @return An object of class `clinicsig`
#' @export
clinical_significance <- function(data, id, time, outcome, measurements = NULL, baseline = NULL, m_functional = NA, sd_functional = NA, type = "a", reliability, better_is = c("lower", "higher")) {
  pre <- post <- clinical_pre <- functional_post <- improved <- detoriorated <- recovered <- unchanged <- NULL
  # Check if arguments are set correctly
  if (missing(id)) stop("You must specify an ID column.")
  if (missing(time)) stop("You must specify a column indicating the different measurements.")
  if (missing(outcome)) stop("You must specify an outcome.")


  # Sanity checks
  if (!missing(measurements) & length(measurements) != 2) stop("If you specify time levels, you must specify only two.")
  if (!missing(baseline) & length(baseline) != 1) stop("If you specify a baseline measurement, you must specify only one.")


  # Check if all necessary information is provided
  if (type != "a" & (missing(m_functional) | missing(sd_functional))) {
    stop(paste0("To calculate cutoff \"", type, "\", summary statistics for the functional population must be defined."))
  }


  # If type = "a", discard information of the functional population and give a warning
  if (type == "a" & (!is.na(m_functional) | !is.na(sd_functional))) {
    m_functional <- sd_functional <- NA_real_
    warning("You selected cutoff type \"a\" and provided summary statistics for the functional population. This information will be dicarded.\nIf you wand to incorporate data from the functional population for the cutoff, choose type = \"b\" or \"c\"", call. = FALSE)
  }


  # Prepare data
  datasets <- .prep_data(
    data = data,
    id = {{ id }},
    time = {{ time }},
    outcome = {{ outcome }},
    measurements = measurements,
    baseline = baseline
  )


  # Prepare cutoff
  cutoff <- prep_cutoff(
    data = datasets[["data"]],
    m_functional = m_functional,
    sd_functional = sd_functional,
    type = type,
    better_is = better_is
  )


  # Prepare RCI
}
