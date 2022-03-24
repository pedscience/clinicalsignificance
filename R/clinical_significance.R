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
    warning("You selected cutoff type \"a\" and provided summary statistics for the functional population. This information will be dicarded.\nIf you wand to incorporate data from the functional population for the cutoff, choose type = \"b\" or \"c\"", call. = FALSE)
  }


  # Prepare data
  datasets <- prep_data(
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
  rci <- prep_rci(
    data = datasets[["data"]],
    reliability = reliability
  )

  direction <- match.arg(better_is)
  dir_factor <- 1
  if (direction == "higher") dir_factor <- -1

  # Check clinical significance criteria
  criteria <- bind_cols(datasets[["data"]], rci = rci) %>%
    mutate(
      clinical_pre = ifelse(dir_factor * pre > dir_factor * cutoff$cutoff, TRUE, FALSE),
      functional_post = ifelse(dir_factor * post < dir_factor * cutoff$cutoff, TRUE, FALSE),
      improved = ifelse(rci < -1.96, TRUE, FALSE),
      detoriorated = ifelse(rci > 1.96, TRUE, FALSE),
      recovered = clinical_pre & functional_post & improved,
      unchanged = !improved & !detoriorated
    ) %>%
    relocate(recovered, improved, unchanged, detoriorated, .after = functional_post) %>%
    select(id, clinical_pre:detoriorated)

  all_datasets <- c(datasets, criteria = list(criteria))

  clinicsig <- list(
    datasets = all_datasets,
    cutoff = cutoff,
    rci = rci
  )

  class(clinicsig) <- "clinicsig"

  return(clinicsig)
}
