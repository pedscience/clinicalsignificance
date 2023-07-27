cs_combined <- function(data,
                        id,
                        time,
                        outcome,
                        group = NULL,
                        pre = NULL,
                        post = NULL,
                        reliability = NULL,
                        reliability_post = NULL,
                        m_functional = NULL,
                        sd_functional = NULL,
                        better_is = c("lower", "higher"),
                        rci_method = c("JT", "GLN", "HLL", "EN", "NK", "HA", "HLM"),
                        cutoff_type = c("a", "b", "c"),
                        significance_level = 0.05) {
  # Argument checks
  cs_method <- rlang::arg_match(rci_method)
  cut_type <- rlang::arg_match(cutoff_type)

  # Prepare the data
  datasets <- .prep_data(
    data = data,
    id = {{ id }},
    time = {{ time }},
    outcome = {{ outcome }},
    group = {{ group }},
    pre = {{ pre }},
    post = {{ post }},
    method = cs_method
  )


  # Prepend a class to enable method dispatch for RCI calculation
  class(datasets) <- c(paste0("cs_", tolower(cs_method)), class(datasets))


  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )


  # Calculate relevant summary statistics for the chosen RCI method
  m_pre <- mean(datasets[["data"]][["pre"]])
  sd_pre <- stats::sd(datasets[["data"]][["pre"]])
  if (cs_method %in% c("HLL", "HA")) {
    m_post <- mean(datasets[["data"]][["post"]])
    sd_post <- stats::sd(datasets[["data"]][["post"]])
  }


  # Get the direction of a beneficial intervention effect
  if (rlang::arg_match(better_is) == "lower") direction <- -1 else direction <- 1


  # Determine critical RCI value based on significance level
  if (cs_method != "HA") critical_value <- stats::qnorm(1 - significance_level/2) else critical_value <- stats::qnorm(1 - significance_level)


  # Determine RCI and check each participant's change relative to it
  rci_results <- calc_rci(
    data = datasets,
    m_pre = m_pre,
    m_post = m_post,
    sd_pre = sd_pre,
    sd_post = sd_post,
    reliability = reliability,
    reliability_post = reliability_post,
    direction = direction,
    critical_value = critical_value
  )


  # Calculate the cutoff value and check each patient's change relative to it
  cutoff_results <- calc_cutoff_from_data(
    data = datasets,
    m_clinical = m_pre,
    sd_clinical = sd_pre,
    m_functional = m_functional,
    sd_functional = sd_functional,
    m_post = m_post,
    sd_post = sd_post,
    reliability = reliability,
    type = cut_type,
    direction = direction,
    critical_value = critical_value
  )

  class(rci_results) <- c("cs_combined", "list")



  # Create the summary table for printing and exporting



  # class(rci_results) <- "list"
  # class(cutoff_results) <- "list"


  list(
    data = datasets,
    direction = direction,
    cutoff_results = cutoff_results,
    rci_results = rci_results
  )
}
