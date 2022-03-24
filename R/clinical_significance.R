clinical_significance <- function(data, id, time, outcome, measurements = NULL, baseline = NULL, m_functional = NA, sd_functional = NA, type = "a", reliability, better_is = c("lower", "higher")) {
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
    warning("You selected cutoff type \"a\" and provided summary statistics for the functional population. This information will be dicarded.\nIf you wand to incorporate data from the functional population, choose type = \"b\" or \"c\"", call. = FALSE)
  }


  # Prepare data
  tidy_data <- prep_data(
    data = data,
    id = {{ id }},
    time = {{ time }},
    outcome = {{ outcome }},
    measurements = measurements,
    baseline = baseline
  )


  # Prepare cutoff
  cutoff <- prep_cutoff(
    data = tidy_data[["data"]],
    m_functional = m_functional,
    sd_functional = sd_functional,
    type = type,
    better_is = better_is
  )


  # Prepare RCI
  rci <- prep_rci(
    data = tidy_data[["data"]],
    reliability = reliability
  )


  list(
    data = tidy_data,
    cutoff = cutoff,
    rci = rci
  )
}
