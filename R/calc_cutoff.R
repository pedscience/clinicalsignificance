#' Calculate the cutoff criterion from a data frame
#'
#' @param data A preprocessed wide dataframe with at least column `pre` and
#'   `post`
#' @param m_functional Mean of functional population
#' @param sd_functional SD of functional population
#' @param type Cutoff type
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @importFrom stats sd relevel
#' @importFrom rlang .data
#'
#' @return A list with cutoff info and participant wise info on cutoff
#'   categorization
#'
#' @noRd
.calc_cutoff_data <- function(data, m_clinical, sd_clinical, m_functional, sd_functional, type = "a", direction = 1) {
  # Calculate cutoff
  cutoff_info <- .calc_cutoff(
    m_clinical = m_clinical,
    sd_clinical = sd_clinical,
    m_functional = m_functional,
    sd_functional = sd_functional,
    type = type,
    direction = direction
  )

  data_cutoff_criteria <- data %>%
    mutate(
      clinical_pre    = ifelse(direction * .data$pre < direction * cutoff_info[["value"]], TRUE, FALSE),
      functional_post = ifelse(direction * .data$post > direction * cutoff_info[["value"]], TRUE, FALSE),
    ) %>%
    select(.data$id, .data$clinical_pre, .data$functional_post)

  # Bind cutoff info and data with cutoff criteria together for further
  # calculations
  list(
    info = cutoff_info,
    data = data_cutoff_criteria
  )
}


#' Calculate Cutoff Criterion Using Summary Values
#'
#' @param m_clinical Mean of clinical population.
#' @param sd_clinical SD of clinical population.
#' @param m_functional Mean of functional population.
#' @param sd_functional SD of functional population.
#' @param type Cutoff type
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @return A list with elements `m_clinical`, `sd_clinical`, `m_function`,
#'   `sd_functional`, `type`, and `value`
#'
#' @noRd
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
    value = cutoff
  )
}
