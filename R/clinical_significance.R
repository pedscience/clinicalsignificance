#' Clinical Significance
#'
#' @param data A tidy data frame
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
clinical_significance <- function(data, id, time, outcome, measurements = NULL, baseline = NULL, m_functional = NA, sd_functional = NA, type = "a", reliability, better_is = c("lower", "higher"), method = c("JT", "GLN")) {
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

  n_obs <- nrow(datasets[["data"]])


  # Calculate cutoff
  direction <- 1
  if (match.arg(better_is) == "lower") direction <- -1

  cutoff <- .calc_cutoff_data(
    data = datasets[["data"]],
    m_functional = m_functional,
    sd_functional = sd_functional,
    type = type,
    direction = direction
  )

  clinisig_method <- match.arg(method)

  # Calculate RCI
  if (clinisig_method == "JT") {
    rci <- .calc_rci_jacobson(
      data = datasets[["data"]],
      reliability = reliability
    )
  } else if (clinisig_method == "GLN") {
    rci <- .calc_rci_gulliksen(
      data = datasets[["data"]],
      reliability = reliability
    )
  }


  # Calculate categories
  categories <- .calc_categories_jacobson(
    data = datasets[["data"]],
    cutoff = cutoff,
    rci = rci,
    direction = direction
  )

  summary_table <- .create_summary_table(
    data = categories,
    n_participants = n_obs
  )


  # Results
  out <- list(
    datasets = datasets,
    n_obs = n_obs,
    cutoff = cutoff,
    rci = rci,
    categories = categories,
    summary = summary_table
  )

  class(out) <- "clinisig"
  return(out)
}


#' Print Clinical Significance Results
#'
#' @param x A clinicsig object
#' @param ... Additional arguments
#'
#' @importFrom insight export_table
#'
#' @export
print.clinisig <- function(x, ...) {
  cat(export_table(x$summary, width = c(n = 5), digits = 3, ...))
}



#' Summary Method for a clinisig object
#'
#' @param object A clinisig object
#' @param ... Additional arguments
#'
#' @export
summary.clinisig <- function(object, ...) {
  object[["summary"]]
}
