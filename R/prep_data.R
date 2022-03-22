prep_data <- function(data, id, time, outcome, measurements = NULL, baseline = NULL) {
  # Check if arguments are set correctly
  if (missing(id)) stop("You must specify an ID column.")
  if (missing(time)) stop("You must specify a column indicating the different measurements.")
  if (missing(outcome)) stop("You must specify an outcome.")


  # Sanity checks
  if (!missing(measurements) & length(measurements) != 2) stop("If you specify time levels, you must specify only two.")
  if (!missing(baseline) & length(baseline) != 1) stop("If you specify a baseline measurement, you must specify only one.")


  # Select relevant variables and filter relevant levels (if defined)
  selected_data <- data %>%
    select(id = {{ id }}, time = {{ time }}, outcome = {{ outcome }}) %>%
    filter(time %in% measurements)


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
      ungroup() %>%
      select(id, time, outcome)

    if (is.null(baseline)) {
      sorted_levels <- pull(sorted_data, time) %>% levels()

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
