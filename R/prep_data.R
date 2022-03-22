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
#' the `time` column is of type character, you must provide a string for
#' `baseline` (i.e., the pre measurement) because characters cannot be sorted in
#' a meansingful way.
#'
#' @param data A tidy dataframe
#' @param id Participant ID
#' @param time Time variable
#' @param outcome Outcome variable
#' @param measurements If `time` contains more than two values, you can specify
#'   your two measurements of interest as a vector
#' @param baseline If your `time` is of type character, you can specify the pre
#'   measurement
#'
#' @importFrom stats na.omit
#' @importFrom dplyr select filter mutate group_by ungroup arrange rename pull
#'   distinct last_col
#' @importFrom tidyr pivot_wider
#' @importFrom forcats fct_relevel
#'
#' @return A wide dataframe containing the ID, pre and post scores, and the
#'   change score
#'
#' @examples
#' prep_data(jacobson_1989, subject, time, das, baseline = "pre")
#' @export
prep_data <- function(data, id, time, outcome, measurements = NULL, baseline = NULL) {
  data <- data
  pre <- post <- NULL
  # Check if arguments are set correctly
  if (missing(id)) stop("You must specify an ID column.")
  if (missing(time)) stop("You must specify a column indicating the different measurements.")
  if (missing(outcome)) stop("You must specify an outcome.")


  # Sanity checks
  if (!missing(measurements) & length(measurements) != 2) stop("If you specify time levels, you must specify only two.")
  if (!missing(baseline) & length(baseline) != 1) stop("If you specify a baseline measurement, you must specify only one.")


  # Select relevant variables and filter relevant levels (if defined)
  selected_data <- data %>%
    select(id = {{ id }}, time = {{ time }}, outcome = {{ outcome }})


  # If measurements are defined, filter data accordingly
  if (!missing(measurements)) {
    selected_data %>%
      filter(time %in% measurements)
  }


  # If the time column contains multiple measurements, throw an error and
  # request specification of two measurements that should be used
  distinct_measurements <- selected_data %>%
    distinct(time) %>%
    nrow()

  if (distinct_measurements != 2) stop("Your measurement column contains more than two measurements. Please specify which two measurements should be used.")


  # Make sure that the data is sorted correctly (the baseline measurement should
  # always be sorted before any subsequent measurement)
  if (is.numeric(pull(selected_data, time))) {
    sorted_data <- selected_data %>%
      group_by(id) %>%
      arrange(id, time) %>%
      ungroup()
  } else if (is.character(pull(selected_data, time))) {
    sorted_data <- selected_data %>%
      mutate(time = fct_relevel(time, baseline)) %>%
      group_by(id) %>%
      arrange(id, time) %>%
      ungroup()
      # select(id, time, outcome)

    if (is.null(baseline)) {
      sorted_levels <- levels(pull(sorted_data, time))

      warning_message <- paste0("Your pre measurement is \"", sorted_levels[1], "\" and your post measurement is \"", sorted_levels[2], "\".\n If that is not correct, please specify the baseline.")

      warning(warning_message, call. = FALSE)
    }
  }

  # Make the data wide
  wide_data <- sorted_data %>%
    pivot_wider(
      names_from = time,
      values_from = outcome
    ) %>%
    rename(pre = last_col() - 1, post = last_col())

  wide_data %>%
    na.omit() %>%
    mutate(
      change = post - pre
    )
}
