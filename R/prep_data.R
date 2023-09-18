#' Prepare Data for Clinical Significance Analyses
#'
#' Prepare a tidy dataset for analyses of clinical significance. This function
#' will select only the relevant variables and spread the measurement column out
#' to make the data wide which is more optimal for such analyses. Additionally,
#' `prep_data()` calculates the raw change. Furthermore, this function can
#' prepare data for the HLM method, if the method argument is set accordingly.
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
#' @importFrom tidyr pivot_wider
#' @importFrom rlang abort inform
#'
#' @return A list containing the original data frame in wide format and the
#'   useable data frame without missing values
#'
#' @noRd
.prep_data <- function(data, id, time, outcome, group = NULL, pre = NULL, post = NULL, method = "JT") {
  if (method != "HLM") {
    # Select relevant variables
    selected_data <- data |>
      dplyr::select(id = {{ id }}, group = {{ group }}, time = {{ time }}, outcome = {{ outcome }}, )


    # If measurements are defined, filter data accordingly
    if (!is.null(pre) & !is.null(post)) {
      selected_data <- selected_data |>
        dplyr::filter(time %in% c(pre, post))
    }


    # If the time column contains multiple measurements, throw an error and
    # request specification of two measurements that should be used
    distinct_measurements <- selected_data |>
      dplyr::distinct(time) |>
      nrow()

    if (distinct_measurements > 2) abort("Your measurement column contains more than two measurements. \nPlease specify which two measurements should be used with arguments \"pre\" and \"post\".")


    # Make sure that the data is sorted correctly (the pre measurement should
    # always be sorted before any subsequent measurement)
    if (is.numeric(selected_data[["time"]])) {
      sorted_data <- selected_data |>
        dplyr::group_by(id) |>
        dplyr::arrange(id, time) |>
        dplyr::ungroup()
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
        dplyr::mutate(time = relevel(as.factor(time), pre)) |>
        dplyr::group_by(id) |>
        dplyr::arrange(id, time) |>
        dplyr::ungroup()
    } else if (is.factor(selected_data[["time"]])) {
      if (is.null(pre)) {
        sorted_levels <- levels(selected_data[["time"]])
        information <- paste0("Your \"", sorted_levels[1], "\" was set as pre measurement and and your \"", sorted_levels[2], "\" as post.")
        inform(c("i" = information), footer = c("*" = "If that is not correct, please specify the pre measurement with the argument \"pre\"."), use_cli_format = TRUE)
        pre <- sorted_levels[[1]]
      }

      # Sort factors
      sorted_data <- selected_data |>
        dplyr::mutate(time = relevel(as.factor(time), pre)) |>
        dplyr::group_by(id) |>
        dplyr::arrange(id, time) |>
        dplyr::ungroup()
    }


    # Make the data wide
    wide_data <- sorted_data |>
      tidyr::pivot_wider(
        names_from = time,
        values_from = outcome
      ) |>
      dplyr::rename(pre = dplyr::last_col() - 1, post = dplyr::last_col())


    # Omit cases with missing values. They can not be used. Calculate raw change
    # score
    used_data <- wide_data |>
      na.omit() |>
      dplyr::mutate(
        change = post - pre
      )

    out <- list(
      original = data,
      wide = wide_data,
      data = used_data
    )
  } else if (method == "HLM") {
    # Select relevant variables
    imported_data <- data |>
      dplyr::select(id = {{ id }}, group = {{ group }}, time = {{ time }}, outcome = {{ outcome }}) |>
      dplyr::mutate(id = as.character(id))

    if (.has_group(imported_data)) {
      groups <- imported_data |>
        dplyr::select(id, group) |>
        dplyr::distinct(id, group)
    } else {
      groups <- NULL
    }


    # Get n of measurements and first (pre) and last (post) measurement
    wide_data <- imported_data |>
      na.omit() |>
      dplyr::summarise(
        n = dplyr::n(),
        pre = dplyr::first(outcome),
        post = dplyr::last(outcome),
        .by = "id"
      )


    # Only include patients with at least three measurements
    if (.has_group(imported_data)) {
      cutoff_data <- wide_data |>
        dplyr::left_join(groups, by = "id") |>
        dplyr::filter(n >= 3) |>
        dplyr::relocate(group, .after = id)
    } else {
      cutoff_data <- wide_data |>
        dplyr::filter(n >= 3)
    }


    # Only use those participants with more than one measurement
    prepped_data <- imported_data |>
      dplyr::filter(id %in% cutoff_data[["id"]])


    # Determine min and max of measurements (needed for plotting)
    min_measurement <- min(prepped_data[["time"]])
    max_measurement <- max(prepped_data[["time"]])

    out <- list(
      original = data,
      wide = wide_data,
      data = cutoff_data,
      groups = groups,
      model = prepped_data,
      min = min_measurement,
      max = max_measurement
    )
  }

  return(out)
}
