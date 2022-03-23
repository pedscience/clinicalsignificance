prep_cutoff <- function(data, m_functional = NA, sd_functional = NA, type = "c", better_is = c("lower", "higher")) {
  # Check if all necessary information is provided
  if (type != "a" & (missing(m_functional) | missing(sd_functional))) {
    stop(paste0("To calculate cutoff ", type, ", summary statistics of the functional population must be defined."))
  }

  # If type = "a", discard information of the functional population and give a warning
  if (type == "a" & (!is.na(m_functional) | !is.na(sd_functional))) {
    m_functional <- sd_functional <- NA_real_
    warning("You selected cutoff type \"a\" and provided summary statistics for the functional population. This information will be dicarded.\nIf you wand to incorporate data from the functional population, choose type = \"b\" or \"c\"", call. = FALSE)
  }


  # If type = "a" or "c", calculate mean and standard deviation based on the
  # data. Otherwise, these will be NA
  m_clinical <- sd_clinical <- NA_real_
  if (type != "b") {
    m_clinical <- mean(data$pre)
    sd_clinical <- sd(data$pre)
  }


  # Calculate cutoff
  .calc_cutoff(
    m_clinical = m_clinical,
    sd_clinical = sd_clinical,
    m_functional = m_functional,
    sd_functional = sd_functional,
    type = type,
    better_is = better_is
  )


}


.calc_cutoff <- function(m_clinical, sd_clinical, m_functional, sd_functional, type, better_is = c("lower", "higher")) {
  # Determine which direction constitutes better values
  direction <- match.arg(better_is)
  direction_factor <- 1
  if (direction == "lower") direction_factor <- -1


  # Calculate cutoffs
  if (type == "a") {
    cutoff <- m_clinical + (direction_factor * 2 * sd_clinical)
  } else if (type == "b") {
    cutoff <- m_functional - (direction_factor * 2 * sd_functional)
  } else if (type == "c") {
    cutoff <- (sd_clinical * m_functional + sd_functional * m_clinical) / (sd_clinical + sd_functional)
  }

  list(
    m_clinical = m_clinical,
    sd_clinical = sd_clinical,
    m_functional = m_functional,
    sd_functional = sd_functional,
    type = type,
    cutoff = cutoff
  )
}
