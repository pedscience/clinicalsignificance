#' Prepare Data for Clinical Significance Analyses
#'
#' Prepare a tidy dataset for analyses of clinical significance. This function
#' will select only the relevant variables and spread the measurement column out
#' to make the data wide which is more optimal for such analyses. Additionally,
#' `prep_data()` calculates the raw change.
#'
#' Be careful to have your measurement column structured correctly or give
#' additional information to return correct pre and post columns. If your `time`
#' column contains more than two values, you must choose the two values for
#' which you would like to calculate the clinical significance (i.e., the pre
#' and post measurement).
#'
#' If the `time` column is numeric, the values will be sorted numerically. If
#' the `time` column is of type character or factor, you must provide a string
#' for `pre` because characters cannot be sorted in a meaningful way.
#'
#' @param data A tidy dataframe
#' @param id Participant ID
#' @param time Time variable
#' @param pre Pre measurement
#' @param post Post measurement
#' @param outcome Outcome variable
#'
#' @importFrom stats na.omit
#' @importFrom dplyr select filter mutate group_by ungroup arrange rename pull
#'   distinct last_col
#' @importFrom tidyr pivot_wider
#' @importFrom rlang abort inform
#'
#' @return A list containing the original data frame in wide format and the
#'   useable data frame without missing values
#'
#' @noRd
.prep_data <- function(data, id, time, outcome, group = NULL, pre = NULL, post = NULL) {
  # Select relevant variables
  selected_data <- data |>
    select(id = {{ id }}, group = {{ group }}, time = {{ time }}, outcome = {{ outcome }}, )


  # If measurements are defined, filter data accordingly
  if (!is.null(pre) & !is.null(post)) {
    selected_data <- selected_data |>
      filter(time %in% c(pre, post))
  }


  # If the time column contains multiple measurements, throw an error and
  # request specification of two measurements that should be used
  distinct_measurements <- selected_data |>
    distinct(time) |>
    nrow()

  if (distinct_measurements > 2) abort("Your measurement column contains more than two measurements. \nPlease specify which two measurements should be used with arguments \"pre\" and \"post\".")


  # Make sure that the data is sorted correctly (the pre measurement should
  # always be sorted before any subsequent measurement)
  if (is.numeric(selected_data[["time"]])) {
    sorted_data <- selected_data |>
      group_by(id) |>
      arrange(id, time) |>
      ungroup()
  } else if (is.character(selected_data[["time"]])) {
    if (is.null(pre)) {
      sorted_levels <- levels(as.factor(selected_data[["time"]]))
      information <- paste0("Your \"", sorted_levels[1], "\" was set as pre measurement and and your \"", sorted_levels[2], "\" as post.")
      inform(information, use_cli_format = TRUE, footer = "If that is not correct, please specify the pre measurement with the argument \"pre\".")

      # If pre is not defined, use first level as baseline
      pre <- sorted_levels[[1]]
    }

    # Sort strings
    sorted_data <- selected_data |>
      mutate(time = relevel(as.factor(time), pre)) |>
      group_by(id) |>
      arrange(id, time) |>
      ungroup()
  } else if (is.factor(selected_data[["time"]])) {
    if (is.null(pre)) {
      sorted_levels <- levels(selected_data[["time"]])
      information <- paste0("Your \"", sorted_levels[1], "\" was set as pre measurement and and your \"", sorted_levels[2], "\" as post.")
      inform(c("i" = information), footer = c("*" = "If that is not correct, please specify the pre measurement with the argument \"pre\"."), use_cli_format = TRUE)
      pre <- sorted_levels[[1]]
    }

    # Sort factors
    sorted_data <- selected_data |>
      mutate(time = relevel(as.factor(time), pre)) |>
      group_by(id) |>
      arrange(id, time) |>
      ungroup()
  }


  # Make the data wide
  wide_data <- sorted_data |>
    pivot_wider(
      names_from = time,
      values_from = outcome
    ) |>
    rename(pre = last_col() - 1, post = last_col())


  # Omit cases with missing values. They can not be used. Calculate raw change
  # score
  used_data <- wide_data |>
    na.omit() |>
    mutate(
      change = post - pre
    )

  list(
    original = data,
    wide = wide_data,
    data = used_data
  )
}


#' Prepare data for HLM method
#'
#' @param data A tidy data frame with at least columns indicating id, time and
#'   an outcome
#' @param id Participant ID
#' @param time Measurement
#' @param outcome Outcome
#'
#' @importFrom dplyr select mutate group_by summarise filter n first last
#'
#' @return A list with original and prepped data
#'
#' @noRd
.prep_data_hlm <- function(data, id, time, outcome, group = NULL) {
  # Select relevant variables
  imported_data <- data |>
    select(id = {{ id }}, group = {{ group }}, time = {{ time }}, outcome = {{ outcome }}) |>
    mutate(id = as.character(id))

  if (.has_group(imported_data)) {
    groups <- imported_data |>
      select(id, group) |>
      distinct(id, group)
  } else {
    groups <- NULL
  }


  # Get n of measurements and first (pre) and last (post) measurement
  wide_data <- imported_data |>
    na.omit() |>
    group_by(id) |>
    summarise(
      n = n(),
      pre = first(outcome),
      post = last(outcome),
      .groups = "drop"
    )


  # Only include patients with at least three measurements
  if (.has_group(imported_data)) {
    cutoff_data <- wide_data |>
      left_join(groups, by = "id") |>
      filter(n >= 3) |>
      relocate(group, .after = id)
  } else {
    cutoff_data <- wide_data |>
      filter(n >= 3)
  }


  # Only use those participants with more than one measurement
  prepped_data <- imported_data |>
    filter(id %in% cutoff_data[["id"]])


  # Determine min and max of measurements (needed for plotting)
  min_measurement <- min(prepped_data[["time"]])
  max_measurement <- max(prepped_data[["time"]])

  list(
    original = data,
    wide = wide_data,
    data = cutoff_data,
    groups = groups,
    model = prepped_data,
    min = min_measurement,
    max = max_measurement
  )
}
