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
#'   `"a"` (see details for further information in which cutoff to choose).
#' @param reliability The instrument's reliability estimate.
#' @param better_is Which direction means a better outcome? Available are
#'   `"lower"` and `"higher"`. Defaults to `"higher"`.
#' @param method Clinical significance method. Available are `\"JT\"` (Jacobson
#'   & Truax) and `\"GLN\"` (Gulliksen, Lord, and Novick)
#'
#' @importFrom dplyr relocate bind_cols
#' @importFrom rlang arg_match
#'
#' @return A S3 object of class `clinisig`
#' @export
clinical_significance <- function(data, id, time, outcome, measurements = NULL, baseline = NULL, m_functional = NA, sd_functional = NA, type = "a", reliability, better_is = c("lower", "higher"), method = c("JT", "GLN", "EN")) {
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

  # If type = "a" or "c", calculate mean and standard deviation based on the
  # data. Otherwise, these will be NA
  m_clinical <- sd_clinical <- NA
  if (type != "b") {
    m_clinical <- mean(datasets[["data"]][["pre"]])
    sd_clinical <- sd(datasets[["data"]][["pre"]])
  }

  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )



  # Calculate cutoff
  direction <- 1
  if (arg_match(better_is) == "lower") direction <- -1

  cutoff <- .calc_cutoff_data(
    data = datasets[["data"]],
    m_clinical = m_clinical,
    sd_clinical = sd_clinical,
    m_functional = m_functional,
    sd_functional = sd_functional,
    type = type,
    direction = direction
  )

  clinisig_method <- arg_match(method)

  # Calculate RCI
  if (clinisig_method == "JT") {
    rci <- .calc_rci_jt(
      data = datasets[["data"]],
      reliability = reliability,
      direction = direction
    )
  } else if (clinisig_method == "GLN") {
    rci <- .calc_rci_gln(
      data = datasets[["data"]],
      reliability = reliability,
      direction = direction
    )
  } else if (clinisig_method == "EN") {
    rci <- .calc_rci_en(
      data = datasets[["data"]],
      reliability = reliability,
      direction = direction
    )
  }


  # Calculate recovered
  categories <- .calc_recovered(
    cutoff_data = cutoff[["data"]],
    rci_data = rci[["data"]]
  )


  # Create a summary table for printing
  summary_table <- .create_summary_table(
    data = categories,
    n_obs = n_obs[["n_used"]]
  )


  # Results
  out <- list(
    datasets = datasets,
    n_obs = n_obs,
    method = method,
    reliability = reliability,
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
  clinisig_method <- x[["method"]]
  if (length(x[["method"]]) > 1) {
    clinisig_method <- x[["method"]][[1]]
  } else {
    clinisig_method <- x[["method"]]
  }

  title_text <- paste0("Clinical Significance Results (", clinisig_method, ")")
  summary_table <- x[["summary"]]

  cat(
    export_table(
      summary_table,
      width = c(n = 5),
      digits = 3,
      caption = c(title_text, "blue"),
      ...
    )
  )
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
