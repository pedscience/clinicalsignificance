#' Calculate the Cutoff Criterion
#'
#' @param data A preprocessed wide dataframe
#' @param m_functional Mean of functional population
#' @param sd_functional SD of functional population
#' @param type Cutoff type
#' @param better_is Direction of beneficial results
#'
#' @importFrom stats sd relevel
.calc_cutoff_data <- function(data, m_functional = NA, sd_functional = NA, type = "a", better_is) {

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


#' Calculate Cutoff Criterion Using Summary Values
#'
#' @param m_clinical Mean of clinical population.
#' @param sd_clinical SD of clinical population.
#' @param m_functional Mean of functional population.
#' @param sd_functional SD of functional population.
#' @param type Cutoff type
#' @param better_is Direction of beneficial results
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
