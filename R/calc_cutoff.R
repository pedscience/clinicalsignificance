#' Calculate the Cutoff Criterion
#'
#' @param data A preprocessed wide dataframe
#' @param m_functional Mean of functional population
#' @param sd_functional SD of functional population
#' @param type Cutoff type
#'
#' @inheritParams .calc_cutoff
#'
#' @importFrom stats sd relevel
.calc_cutoff_data <- function(data, m_functional = NA, sd_functional = NA, type = "a", direction = 1) {
  # If type = "a" or "c", calculate mean and standard deviation based on the
  # data. Otherwise, these will be NA
  m_clinical <- sd_clinical <- NA
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
    direction = direction
  )


}


#' Calculate Cutoff Criterion Using Summary Values
#'
#' @param m_clinical Mean of clinical population.
#' @param sd_clinical SD of clinical population.
#' @param m_functional Mean of functional population.
#' @param sd_functional SD of functional population.
#' @param direction Which direction is beneficial? `1` = higher values are
#'   better, `-1` = lower values are better
#' @param type Cutoff type
.calc_cutoff <- function(m_clinical, sd_clinical, m_functional, sd_functional, type = "a", direction = 1) {
  # Calculate cutoffs based on type and direction
  if (type == "a") {
    cutoff <- m_clinical + (direction * 2 * sd_clinical)
  } else if (type == "b") {
    cutoff <- m_functional - (direction * 2 * sd_functional)
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
