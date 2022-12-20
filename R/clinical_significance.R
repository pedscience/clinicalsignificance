#' Clinical Significance
#'
#' This function conducts a clinical significance analysis by determining which
#' patients changed reliably and also moved from the clinical to the functional
#' population during a study.
#'
#' By default, the Jacobson & Truax (1991) method to determine both criteria is
#' used, but there are other methods implemented (see description of arguments).
#'
#' To calculate the cutoff between populations, it is generally recommended to
#' use cutoff `"c"`, thus, incorporating information of the clinical and
#' functional population into the cutoff calculation (regardless of the employed
#' method).
#'
#' During this analysis, a patient can be classified in one of five categories:
#' - Recovered (demonstrated a reliable change in the desired direction and
#' belonged to the clinical population before and to the functional population
#' after intervention)
#' - Improved (demonstrated a reliable change in the desired direction but is
#' still in the same population after intervention as compared to before)
#' - Unchanged (did not demonstrate a reliable change)
#' - Deteriorated (demonstrated a reliable change in the undesired direction but
#' is still in the same population after intervention as compared to before)
#' - Harmed (demonstrated a reliable change in the undesired direction and
#' belonged to the functional population before and to the clinical population
#' after intervention)
#'
#' @param data A tidy data frame
#' @param id Participant ID
#' @param time Time variable
#' @param outcome Outcome variable
#' @param group Grouping variable (optional)
#' @param pre Pre measurement (only needed if the time variable contains more
#'   than two measurements)
#' @param post Post measurement (only needed if the time variable contains more
#'   than two measurements)
#' @param m_functional Mean of the functional population
#' @param sd_functional Standard deviation of the functional population
#' @param type Cutoff type. Available are `"a"`, `"b"`, and `"c"`. Defaults to
#'   `"a"` (see details for further information in which cutoff to choose)
#' @param reliability The instrument's reliability estimate. If you selected the
#'   NK method, the here specified reliability will be the instrument's pre
#'   measurement reliability
#' @param reliability_post The instrument's reliability at post measurement
#'   (only needed for the NK method)
#' @param better_is Which direction means a better outcome for the employed
#'   outcome? Available are
#'   - `"lower"` (lower outcome scores are desirable, the default) and
#'   - `"higher"` (higher outcome scores are desirable)
#' @param method Clinical significance method. Available are
#'   - `"JT"` (Jacobson & Truax, 1991, the default)
#'   - `"GLN"` (Gulliksen, Lord, and Novick; Hsu, 1989, Hsu, 1995)
#'   - `"HLL"` (Hsu, Linn & Nord; Hsu, 1989)
#'   - `"EN"` (Edwards & Nunnally; Speer, 1992)
#'   - `"NK"` (Nunnally & Kotsch, 1983), requires a reliability estimate at post
#'      measurement. If this is not supplied, reliability and reliability_post
#'      are assumed to be equal
#'    - `"HA"` (Hageman & Arrindell, 1999)
#'    - `"HLM"` (Hierarchical Linear Modeling; Raudenbush & Bryk, 2002), requires
#'      at least three measurements per patient
#'
#' @references
#' - Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical approach to defining meaningful change in psychotherapy research. Journal of Consulting and Clinical Psychology, 59(1), 12–19. https://doi.org/10.1037//0022-006X.59.1.12
#' - Hsu, L. M. (1989). Reliable changes in psychotherapy: Taking into account regression toward the mean. Behavioral Assessment, 11(4), 459–467.
#' - Hsu, L. M. (1995). Regression toward the mean associated with measurement error and the identification of improvement and deterioration in psychotherapy. Journal of Consulting and Clinical Psychology, 63(1), 141–144. https://doi.org/10.1037//0022-006x.63.1.141
#' - Speer, D. C. (1992). Clinically significant change: Jacobson and Truax (1991) revisited. Journal of Consulting and Clinical Psychology, 60(3), 402–408. https://doi.org/10.1037/0022-006X.60.3.402
#' - Nunnally, J. C., & Kotsch, W. E. (1983). Studies of individual subjects: Logic and methods of analysis. British Journal of Clinical Psychology, 22(2), 83–93. https://doi.org/10.1111/j.2044-8260.1983.tb00582.x
#' - Hageman, W. J., & Arrindell, W. A. (1999). Establishing clinically significant change: increment of precision and the distinction between individual and group level analysis. Behaviour Research and Therapy, 37(12), 1169–1193. https://doi.org/10.1016/S0005-7967(99)00032-7
#' - Raudenbush, S. W., & Bryk, A. S. (2002). Hierarchical Linear Models - Applications and Data Analysis Methods (2nd ed.). Sage Publications.
#'
#' @importFrom dplyr relocate bind_cols
#' @importFrom rlang arg_match abort warn
#' @importFrom checkmate assert_number
#'
#' @return An S3 object of class `clinisig`
#' @export
#'
#' @examples
#' # Clinical significane for "negative" outcomes (lower values are desirable)
#' jacobson_1989 %>%
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = gds,
#'     pre = "pre",
#'     reliability = 0.80
#'   )
#'
#'
#' # Clinical significane for "positive" outcomes (higher values are desirable)
#' jacobson_1989 %>%
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = das,
#'     pre = "pre",
#'     reliability = 0.80,
#'     better_is = "higher"
#'   )
#'
#'
#' # Clinical significance incorporating descriptives of a functional population.
#' # Make sure to select type = "c" to incorporate the specified functional
#' # descriptives.
#' jacobson_1989 %>%
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = gds,
#'     pre = "pre",
#'     reliability = 0.80,
#'     m_functional = 30,
#'     sd_functional = 7,
#'     type = "c"
#'   )
#'
#'
#' # Change the clinical significance method
#' jacobson_1989 %>%
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = gds,
#'     pre = "pre",
#'     reliability = 0.80,
#'     m_functional = 30,
#'     sd_functional = 7,
#'     type = "c",
#'     method = "EN"
#'   )
#'
#' jacobson_1989 %>%
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = gds,
#'     pre = "pre",
#'     reliability = 0.80,
#'     m_functional = 30,
#'     sd_functional = 7,
#'     type = "c",
#'     method = "HA"
#'   )
#'
#'
#' # And plot your results
#' results <- jacobson_1989 %>%
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = gds,
#'     pre = "pre",
#'     reliability = 0.80,
#'     m_functional = 30,
#'     sd_functional = 7,
#'     type = "c"
#'   )
#'
#' plot(results)
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
      data = datasets[["model"]],
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
      sd_post = sd_post,
      direction = direction
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
#' @param x A clinisig object
#' @param ... Additional arguments passed to `export_table()`
#'
#' @importFrom insight export_table
#' @importFrom dplyr rename_with
#' @importFrom tools toTitleCase
#' @importFrom rlang .data
#'
#' @return No return value, called for side effects
#' @export
print.clinisig <- function(x, ...) {
  clinisig_method <- get_method(x)

  if (clinisig_method != "HA") {
    caption <- c(paste0("Clinical Significance Results (", clinisig_method, ")"), "blue")
    summary_table <- get_summary_table(x) %>%
      rename_with(toTitleCase, .cols = -n)
  }

  if (clinisig_method == "HA") {
    summary_table_individual <- get_summary_table(x, "individual") %>%
      rename_with(toTitleCase, .cols = -n)
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
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' claus_results <- clinical_significance(
#'   data = claus_2020,
#'   id = id,
#'   time = time,
#'   outcome = bdi,
#'   pre = 1,
#'   post = 4,
#'   reliability = 0.801,
#'   m_functional = 8,
#'   sd_functional = 7,
#'   type = "c"
#' )
#'
#' summary(claus_results)
summary.clinisig <- function(object, ...) {
  # Get all bits and pieces to show
  nobs <- get_n(object)
  outcome <- object[["outcome"]]
  clinisig_method <- get_method(object)
  cutoff <- get_cutoff(object)
  reliability <- get_reliability(object)[[1]]
  direction <- get_beneficial_direction(object)
  if (.has_group(get_data(object))) col_alignment <- "llrr" else col_alignment <- "lrr"


  # Cutoff table
  cutoff_descriptives <- get_cutoff_descriptives(object) %>%
    rename(
      "M Clinical" = m_clinical,
      "SD Clinical" = sd_clinical,
      "M Functional" = m_functional,
      "SD Functional" = sd_functional
    ) %>%
    export_table(
      caption = c("Population Characteristics", "blue"),
      digits = 2,
      missing = "---",
      align = "llll")

  if (clinisig_method == "HA") {
    cutoff_descriptives <- get_cutoff_descriptives(object) %>%
      rename(
        "M Clinical" = m_clinical,
        "SD Clinical" = sd_clinical,
        "M Functional" = m_functional,
        "SD Functional" = sd_functional,
        "Reliability Clinical" = reliability_clinical,
        "Reliability Functional" = reliability_functional
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
      rename_with(toTitleCase, .cols = -n) %>%
      export_table(
        caption = c("Individual Level Results", "blue"),
        align = col_alignment,
        digits = 3
      )
  } else  {
    summary_table_individual <- get_summary_table(object, "individual") %>%
      rename_with(toTitleCase, .cols = -n)

    summary_table_group <- get_summary_table(object, "group") %>%
      rename_with(toTitleCase)

    summary_table <- export_table(
      list(summary_table_individual, summary_table_group),
      caption = list(c("Individual Level Results", "blue"), c("Group Level Results", "blue")),
      align = col_alignment,
      digits = 3
    )
  }

  str_participants <- paste0("There were ", bold(nobs[["n_original"]]), " participants in the whole dataset of which ", bold(nobs[["n_used"]]), bold(paste0(" (", round(nobs[["percent_used"]], digits = 3) * 100, "%)")), " could be included in the analysis.")
  str_method <- paste0("The ", bold(clinisig_method), " method for calculating cutoffs and reliable change was chosen and the outcome variable was ", bold(paste0("\"", outcome ,"\"")), ".")
  str_cutoff <- paste0("The cutoff type was ", bold(paste0("\"", cutoff[["type"]], "\"")), " with a value of ", bold(round(cutoff[["value"]], digits = 2)), " based on the following population characteristics (with ", bold(direction), " values representing a beneficial outcome)", ":")

  # Cat the summary
  cat(blue("\nClinical Significance Results\n\n"))
  # cat("-----------------------------\n")
  cat(strwrap(str_participants, width = 70), sep = "\n")
  cat("\n")
  cat(strwrap(str_method, width = 70), sep = "\n")
  cat("\n")
  cat(strwrap(str_cutoff, width = 70), sep = "\n")
  cat("\n", cutoff_descriptives, "\n\n", sep = "")
  if (!is.na(reliability)) {
    cat("The instrument's reliability was set to ", bold(round(reliability, digits = 2))," \n\n", sep = "")
  } else {
    cat("The instrument's reliability was not specified.\n\n")
  }
  cat(summary_table)
}
