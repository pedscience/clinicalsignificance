#' Generic for statistical approach
#'
#' @param x
#'
#' @return Cutoff results
#' @export
#'
#' @noRd
calc_cutoff_from_data <- function(x,
                                  data,
                                  m_clinical,
                                  sd_clinical,
                                  m_functional,
                                  sd_functional,
                                  m_post,
                                  sd_post,
                                  reliability,
                                  type,
                                  direction,
                                  critical_value) {
  UseMethod("calc_cutoff_from_data")
}


#' Calculate the categories based on the cutoff
#'
#' @param data A preprocessed wide dataframe with at least column `id`, `pre`
#'   and `post`
#' @param m_functional Mean of functional population
#' @param sd_functional SD of functional population
#' @param type Cutoff type, available are `"a"`, `"b"`, and `"c"`
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @importFrom stats sd relevel
#'
#' @return A list with cutoff info and participant wise info on cutoff
#'   categorization
#'
#' @noRd
calc_cutoff_from_data.default <- function(data, m_clinical, sd_clinical, m_functional, sd_functional, type = "a", direction = 1, ...) {
  data <- data[["data"]]

  # Calculate cutoff
  cutoff_info <- .calc_cutoff_jt(
    m_clinical = m_clinical,
    sd_clinical = sd_clinical,
    m_functional = m_functional,
    sd_functional = sd_functional,
    type = type,
    direction = direction
  )

  cutoff <- cutoff_info[["value"]]

  data_cutoff_criteria <- data |>
    dplyr::mutate(
      clinical_pre    = ifelse(direction * pre < direction * cutoff, TRUE, FALSE),
      functional_post = ifelse(direction * post > direction * cutoff, TRUE, FALSE),
    ) |>
    dplyr::select(id, clinical_pre, functional_post)

  # Bind cutoff info and data with cutoff criteria together for further
  # calculations
  out <- list(
    info = cutoff_info,
    direction = direction,
    data = data_cutoff_criteria
  )

  class(out) <- c("cs_statistical", class(out))
  out
}


#' Calculate cs_indiv
#'
#' @param data A preprocessed data frame with at least column `post`
#' @param m_clinical Mean of clinical population
#' @param sd_clinical SD of clinical population
#' @param m_functional Mean of functional population
#' @param sd_functional SD of functional population
#' @param m_post Mean of post measurement
#' @param sd_post SD of post measurement
#' @param reliability Instrument's reliability
#' @param type Cutoff type, available are `"a"`, `"b"`, and `"c"`
#' @param direction Which direction is better? 1 = higher, -1 = lower
#' @param critical_value The critical value for the RCI decision, should be
#'    1.65 if significance_level = 0.05
#'
#' @return A list with cutoff info and participant wise info on cutoff
#'   categorization
#'
#' @noRd
calc_cutoff_from_data.cs_ha <- function(data, m_clinical, sd_clinical, m_functional, sd_functional, m_post, sd_post, reliability, type = "a", direction = 1, critical_value = 1.65) {
  data <- data[["data"]]

  se_measurement <- .calc_se_measurement(sd_pre = sd_clinical, reliability = reliability)
  reliability_clinical <- .calc_reliability_ha(sd = sd_clinical, se_measurment = se_measurement)
  reliability_post <- .calc_reliability_ha(sd = sd_post, se_measurment = se_measurement)
  reliability_functional <- .calc_reliability_ha(sd = sd_functional, se_measurment = se_measurement)

  cutoff_info <- .calc_cutoff_ha(
    m_clinical = m_clinical,
    sd_clinical = sd_clinical,
    reliability_clinical = reliability_clinical,
    m_functional = m_functional,
    sd_functional = sd_functional,
    reliability_functional = reliability_functional,
    type = type,
    direction = direction
  )

  cutoff <- cutoff_info[["value"]]

  data_cutoff_criteria <- data |>
    dplyr::mutate(
      cs_indiv = (m_post + (post - m_post) * reliability_post - cutoff) / (sqrt(reliability_post) * se_measurement),
      clinical_pre = direction * pre < direction * cutoff,
      functional_post = clinical_pre & direction * cs_indiv > critical_value,
      clinical_post = !clinical_pre & direction * cs_indiv < -critical_value
    ) |>
    dplyr::select(id, cs_indiv, clinical_pre, functional_post, clinical_post)

  out <- list(
    info = cutoff_info,
    reliability_post = reliability_post,
    m_post = m_post,
    sd_post = sd_post,
    direction = direction,
    data = data_cutoff_criteria
  )

  class(out) <- c("cs_statistical", class(out))
  out
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
.calc_cutoff_jt <- function(m_clinical, sd_clinical, m_functional, sd_functional, type = "a", direction = 1) {
  # Calculate cutoffs based on type and direction
  if (type == "a") {
    cutoff <- m_clinical + (direction * 2 * sd_clinical)
  } else if (type == "b") {
    cutoff <- m_functional - (direction * 2 * sd_clinical)
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


#' Calculate true cutoffs according to Hagemann and Arridell
#'
#' @param m_clinical Mean of clinical population
#' @param sd_clinical SD of clinical population
#' @param reliability_clinical Reliability of pre scores
#' @param m_functional Mean of functional population
#' @param sd_functional SD of functional population
#' @param reliability_functional Reliability of post scores
#' @param type Cutoff type
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @return A list with cutoff info
#'
#' @noRd
.calc_cutoff_ha <- function(m_clinical, sd_clinical, reliability_clinical, m_functional, sd_functional, reliability_functional, type = "a", direction = 1) {
  # Calculate cutoffs based on type and direction
  if (type == "a") {
    cutoff <- m_clinical + (direction * 2 * sd_clinical * sqrt(reliability_clinical))
  } else if (type == "b") {
    cutoff <- m_functional - (direction * 2 * sd_clinical * sqrt(reliability_clinical))
  } else if (type == "c") {
    cutoff <- (sd_clinical * sqrt(reliability_clinical) * m_functional + sd_functional * sqrt(reliability_functional) * m_clinical) / (sd_clinical * sqrt(reliability_clinical) + sd_functional * sqrt(reliability_functional))
  }

  list(
    m_clinical = m_clinical,
    sd_clinical = sd_clinical,
    m_functional = m_functional,
    sd_functional = sd_functional,
    reliability_clinical = reliability_clinical,
    reliability_functional = reliability_functional,
    type = paste0(type, "_true"),
    value = cutoff
  )
}
