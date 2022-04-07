#' Clinical Significance
#'
#' @param data A tidy data frame
#' @param id Participant ID
#' @param time Time variable
#' @param outcome Outcome variable
#' @param pre Pre measurement
#' @param post Post measurement
#' @param m_functional Mean of the functional population
#' @param sd_functional Standard deviation of the functional population
#' @param type Cutoff type. Available are `"a"`, `"b"`, and `"c"`. Defaults to
#'   `"a"` (see details for further information in which cutoff to choose).
#' @param reliability The instrument's reliability estimate. If you selected the
#'   NK method, the here specified reliability will be the instrument's pre
#'   measurement reliability.
#' @param reliability_post The instrument's reliability at post measurement.
#'   This is only relevant for the NK method.
#' @param better_is Which direction means a better outcome? Available are
#'   `"lower"` and `"higher"`. Defaults to `"lower"`.
#' @param method Clinical significance method. Available are `\"JT\"` (Jacobson
#'   & Truax) and `\"GLN\"` (Gulliksen, Lord, and Novick)
#'
#' @importFrom dplyr relocate bind_cols
#' @importFrom rlang arg_match abort warn
#' @importFrom checkmate assert_number
#'
#' @return A S3 object of class `clinisig`
#' @export
clinical_significance <- function(data, id, time, outcome, group = NULL, pre = NULL, post = NULL, m_functional = NA, sd_functional = NA, type = "a", reliability, reliability_post, better_is = c("lower", "higher"), method = c("JT", "GLN", "HLL", "EN", "NK", "HA", "HLM")) {
  # Check if arguments are set correctly
  clinisig_method <- arg_match(method)
  if (missing(id)) abort("You must specify an ID column.")
  if (missing(time)) abort("You must specify a column indicating the different measurements.")
  if (missing(outcome)) abort("You must specify an outcome.")
  assert_number(reliability, lower = 0, upper = 1)
  if (clinisig_method == "NK" & !missing(reliability_post)) assert_number(reliability_post, lower = 0, upper = 1)
  if (clinisig_method != "NK" & !missing(reliability_post)) inform(c("i" = "You specified a reliability estimate for the post measurement but did not choose the NK method."), footer = c("*" = "Your post measurement reliability estimate will be ignored."), use_cli_format = TRUE)


  # Check if all necessary information is provided
  if (type != "a" & (missing(m_functional) | missing(sd_functional))) {
    abort(paste0("To calculate cutoff \"", type, "\", summary statistics for the functional population must be defined."))
  }


  # If type = "a", discard information of the functional population and give a warning
  if (type == "a" & (!is.na(m_functional) | !is.na(sd_functional))) {
    m_functional <- sd_functional <- NA_real_
    warn("You selected cutoff type \"a\" and provided summary statistics for the functional population. This information will be dicarded.\nIf you wand to incorporate data from the functional population for the cutoff, choose type = \"b\" or \"c\"", call. = FALSE)
  }


  # Prepare data
  datasets <- .prep_data(
    data = data,
    id = {{ id }},
    time = {{ time }},
    outcome = {{ outcome }},
    group = {{ group }},
    pre = pre,
    post = post
  )


  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )


  # Calculate relevant descriptives for the method of choice
  m_pre <- mean(datasets[["data"]][["pre"]])
  sd_pre <- sd(datasets[["data"]][["pre"]])
  if (clinisig_method %in% c("HLL", "HA")) {
    m_post <- mean(datasets[["data"]][["post"]])
    sd_post <- sd(datasets[["data"]][["post"]])
  }


  # Calculate cutoff
  direction <- 1
  if (arg_match(better_is) == "lower") direction <- -1

  if (clinisig_method != "HA") {
    cutoff <- .calc_cutoff_jt_data(
      data = datasets[["data"]],
      m_clinical = m_pre,
      sd_clinical = sd_pre,
      m_functional = m_functional,
      sd_functional = sd_functional,
      type = type,
      direction = direction
    )
  } else if (clinisig_method == "HA") {
    cutoff <- .calc_cutoff_ha_data(
      data = datasets[["data"]],
      m_clinical = m_pre,
      sd_clinical = sd_pre,
      m_functional = m_functional,
      sd_functional = sd_functional,
      m_post = m_post,
      sd_post = sd_post,
      reliability = reliability,
      type = type,
      direction = direction
    )
  }


  # Calculate RCI
  if (clinisig_method == "JT") {
    rci <- .calc_rci_jt(
      data = datasets[["data"]],
      sd_pre = sd_pre,
      reliability = reliability,
      direction = direction
    )
  } else if (clinisig_method == "GLN") {
    rci <- .calc_rci_gln(
      data = datasets[["data"]],
      m_pre = m_pre,
      sd_pre = sd_pre,
      reliability = reliability,
      direction = direction
    )
  } else if (clinisig_method == "HLL") {
    rci <- .calc_rci_hll(
      data = datasets[["data"]],
      m_pre = m_pre,
      sd_pre = sd_pre,
      m_post = m_post,
      reliability = reliability,
      direction = direction
    )
  } else if (clinisig_method == "EN") {
    rci <- .calc_rci_en(
      data = datasets[["data"]],
      m_pre = m_pre,
      sd_pre = sd_pre,
      reliability = reliability,
      direction = direction
    )
  } else if (clinisig_method == "NK") {
    # Check if post measurement reliability is given. If not, inform and take
    # pre measurement reliability
    if (missing(reliability_post)) {
      reliability_post <- reliability
      inform(c("i" = "The NK method requires reliability estimates for pre and post measurements."), footer = c("*" = "You can specify the post reliability with the \"reliabilit_post\" argument. For now, reliability post was set to reliability pre."), use_cli_format = TRUE)
    }


    rci <- .calc_rci_nk(
      data = datasets[["data"]],
      m_pre = m_pre,
      sd_pre = sd_pre,
      reliability_pre = reliability,
      reliability_post = reliability_post,
      direction = direction
    )
  } else if (clinisig_method == "HA") {
    rci <- .calc_rci_ha(
      data = datasets[["data"]],
      m_pre = m_pre,
      sd_pre = sd_pre,
      m_post = m_post,
      sd_post = sd_post,
      reliability = reliability,
      direction = direction
    )
  }


  # Calculate recovered
  if (clinisig_method != "HA") {
    categories <- .calc_recovered(
      data = datasets[["data"]],
      cutoff_data = cutoff[["data"]],
      rci_data = rci[["data"]]
    )
  } else if (clinisig_method == "HA") {
    categories <- .calc_recovered_ha(
      data = datasets[["data"]],
      cutoff_data = cutoff[["data"]],
      rci_data = rci[["data"]]
    )
  }


  # Create a summary table for printing
  summary_table <- .create_summary_table(
    data = categories,
    n_obs = n_obs[["n_used"]]
  )


  # If method is HA, include group level summary table
  if (clinisig_method == "HA") {
    group_level_summary <- .create_summary_table_ha(
      data = datasets[["data"]],
      r_dd = rci[["r_dd"]],
      se_measurement = rci[["se_measurement"]],
      cutoff = cutoff[["info"]][["value"]],
      sd_post = sd_post
    )

    summary_table <- list(summary_table, group_level_summary)
  }


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
#' @param ... Additional arguments passed to `export_table()`
#'
#' @importFrom insight export_table
#'
#' @export
print.clinisig <- function(x, ...) {
  clinisig_method <- get_clinical_significance_method(x)

  caption <- c(paste0("Clinical Significance Results (", clinisig_method, ")"), "blue")
  summary_table <- x[["summary"]]

  if (clinisig_method == "HA") {
    caption <- list(
      c("Clinical Significance Results (HA Individual Level)", "blue"),
      c("Clinical Significance Results (HA Group Level)", "blue")
    )
  }

  if (.has_group(get_data(x))) alignment <- "llrr" else alignment <- "lrr"

  cat(
    export_table(
      summary_table,
      width = c(n = 5),
      digits = 3,
      caption = caption,
      align = alignment,
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
