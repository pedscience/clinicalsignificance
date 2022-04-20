#' Clinical Significance
#'
#' @param data A tidy data frame
#' @param id Participant ID
#' @param time Time variable
#' @param outcome Outcome variable
#' @param group Grouping variable
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
#' @param method Clinical significance method. Available are
#'  - `"JT"` (Jacobson & Truax)
#'  - `"GLN"` (Gulliksen, Lord, and Novick)
#'
#' @importFrom dplyr relocate bind_cols
#' @importFrom rlang arg_match abort warn
#' @importFrom checkmate assert_number
#'
#' @return An S3 object of class `clinisig`
#' @export
clinical_significance <- function(data,
                                  id,
                                  time,
                                  outcome,
                                  group = NULL,
                                  pre = NULL,
                                  post = NULL,
                                  m_functional = NA,
                                  sd_functional = NA,
                                  type = "a",
                                  reliability,
                                  reliability_post,
                                  better_is = c("lower", "higher"),
                                  method = c("JT", "GLN", "HLL", "EN", "NK", "HA", "HLM")) {
  # Check if arguments are set correctly
  clinisig_method <- arg_match(method)
  if (missing(id)) abort("You must specify an ID column.")
  if (missing(time)) abort("You must specify a column indicating the different measurements.")
  if (missing(outcome)) abort("You must specify an outcome.")
  if (clinisig_method != "HLM") assert_number(reliability, lower = 0, upper = 1) else reliability <- NA
  if (clinisig_method == "NK" & !missing(reliability_post)) assert_number(reliability_post, lower = 0, upper = 1)
  if (clinisig_method != "NK" & !missing(reliability_post)) inform(c("i" = "You specified a reliability estimate for the post measurement but did not choose the NK method."), footer = c("*" = "Your post measurement reliability estimate will be ignored."), use_cli_format = TRUE)


  # Check if all necessary information is provided
  if (type != "a" & (missing(m_functional) | missing(sd_functional))) {
    abort(paste0("To calculate cutoff \"", type, "\", summary statistics for the functional population must be defined."))
  }


  # If type = "a", discard information of the functional population and give a warning
  if (type == "a" & (!is.na(m_functional) | !is.na(sd_functional))) {
    m_functional <- sd_functional <- NA_real_
    inform(c("i" = "You selected cutoff type \"a\" and provided summary statistics for the functional population. This information will be dicarded."), footer = c("*" = "If you wand to incorporate data from the functional population for the cutoff, choose type = \"b\" or \"c\""), use_cli_format = TRUE)
  }


  # Prepare data
  if (clinisig_method == "HLM") {
    datasets <- .prep_data_hlm(
      data = data,
      id = {{ id }},
      time = {{ time }},
      outcome = {{ outcome }},
      group = {{ group }}
    )
  } else {
    datasets <- .prep_data(
      data = data,
      id = {{ id }},
      time = {{ time }},
      outcome = {{ outcome }},
      group = {{ group }},
      pre = pre,
      post = post
    )
  }


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
      inform(
        c("i" = "The NK method requires reliability estimates for pre and post measurements."),
        footer = c("*" = "You can specify the post reliability with the \"reliabilit_post\" argument. For now, reliability post was set to reliability pre."),
        use_cli_format = TRUE
      )
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
  } else if (clinisig_method == "HLM") {
    rci <- .calc_rci_hlm(
      data = datasets[["model_data"]],
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
    data = categories
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
    outcome = deparse(substitute(outcome)),
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
#' @importFrom dplyr rename_with
#' @importFrom tools toTitleCase
#' @importFrom rlang .data
#'
#' @export
print.clinisig <- function(x, ...) {
  clinisig_method <- get_clinical_significance_method(x)

  if (clinisig_method != "HA") {
    caption <- c(paste0("Clinical Significance Results (", clinisig_method, ")"), "blue")
    summary_table <- get_summary_table(x) %>%
      rename_with(toTitleCase, .cols = -.data$n)
  }

  if (clinisig_method == "HA") {
    summary_table_individual <- get_summary_table(x, "individual") %>%
      rename_with(toTitleCase, .cols = -.data$n)
    summary_table_group <- get_summary_table(x, "group") %>%
      rename_with(toTitleCase)

    summary_table <- list(summary_table_individual, summary_table_group)

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
#' @importFrom dplyr rename
#' @importFrom rlang .data
#' @importFrom crayon bold blue
#'
#' @export
summary.clinisig <- function(object, ...) {
  # Get all bits and pieces to show
  nobs <- get_n(object)
  outcome <- object[["outcome"]]
  clinisig_method <- get_clinical_significance_method(object)
  cutoff <- get_cutoff(object)
  reliability <- get_reliability(object)[[1]]
  direction <- get_beneficial_direction(object)
  if (.has_group(get_data(object))) col_alignment <- "llrr" else col_alignment <- "lrr"


  # Cutoff table
  cutoff_descriptives <- get_cutoff_descriptives(object) %>%
    rename(
      "M Clinical" = .data$m_clinical,
      "SD Clinical" = .data$sd_clinical,
      "M Functional" = .data$m_functional,
      "SD Functional" = .data$sd_functional
    ) %>%
    export_table(
      caption = c("Population Characteristics", "blue"),
      digits = 2,
      missing = "---",
      align = "llll")

  if (clinisig_method == "HA") {
    cutoff_descriptives <- get_cutoff_descriptives(object) %>%
      rename(
        "M Clinical" = .data$m_clinical,
        "SD Clinical" = .data$sd_clinical,
        "M Functional" = .data$m_functional,
        "SD Functional" = .data$sd_functional,
        "Reliability Clinical" = .data$reliability_clinical,
        "Reliability Functional" = .data$reliability_functional
      ) %>%
      export_table(
        caption = c("Population Characteristics", "blue"),
        digits = 2,
        missing = "---",
        align = "llllll"
      )
  }


  # Summary table
  if (clinisig_method != "HA") {
    summary_table <- get_summary_table(object) %>%
      rename_with(toTitleCase, .cols = -.data$n) %>%
      export_table(
        caption = c("Individual Level Results", "blue"),
        align = col_alignment,
        digits = 3
      )
  } else  {
    summary_table_individual <- get_summary_table(object, "individual") %>%
      rename_with(toTitleCase, .cols = -.data$n)

    summary_table_group <- get_summary_table(object, "group") %>%
      rename_with(toTitleCase)

    summary_table <- export_table(
      list(summary_table_individual, summary_table_group),
      caption = list(c("Individual Level Results", "blue"), c("Group Level Results", "blue")),
      align = col_alignment,
      digits = 3
    )
  }


  # Cat the summary
  cat(blue("\nClinical Significance Results\n\n"))
  # cat("-----------------------------\n")
  cat("There were ", bold(nobs[["n_original"]]), " participants in the whole dataset of which ", bold(nobs[["n_used"]]), bold(paste0(" (", round(nobs[["percent_used"]], digits = 3) * 100, "%)")), " could be included in the analysis.\n\n", sep = "")
  cat("The ", bold(clinisig_method), " method for calculating cutoffs and reliable change was chosen and the outcome variable was ", bold(paste0("\"", outcome ,"\"")), ".\n\n", sep = "")
  cat("The cutoff type was ", bold(paste0("\"", cutoff[["type"]], "\"")), " with a value of ", bold(round(cutoff[["value"]], digits = 2)), " based on the following population characteristics:\n", sep = "")
  cat("(with ", bold(direction), " values representing a beneficial outcome)\n", sep = "")
  cat("\n", cutoff_descriptives, "\n\n", sep = "")
  if (!is.na(reliability)) {
    cat("The instrument's reliability was set to ", bold(round(reliability, digits = 2))," \n\n", sep = "")
  } else {
    cat("The instrument's reliability was not specified.\n\n")
  }
  cat(summary_table)
}
