# Prepare correct data transformation
correct_data_claus <- claus_2020 |>
  dplyr::select(id, time, bdi) |>
  dplyr::filter(time %in% c(1, 4)) |>
  tidyr::pivot_wider(
    names_from = time,
    values_from = bdi,
    names_prefix = "t_"
  ) |>
  dplyr::rename(pre = t_1, post = t_4) |>
  dplyr::mutate(change = post - pre) |>
  stats::na.omit()


correct_data_jacobson <- jacobson_1989 |>
  dplyr::select(id = subject, time, gds) |>
  tidyr::pivot_wider(
    names_from = time,
    values_from = gds
  ) |>
  dplyr::mutate(change = post - pre) |>
  stats::na.omit()


# Factor sorting test
factor_wide_data <- jacobson_1989 |>
  dplyr::mutate(
    time = factor(time, levels = c("pre", "post"))
  ) |>
  .prep_data(subject, time, gds, pre = "pre", method = "JT") |>
  purrr::pluck("wide")



# HLM Data
imported_data <- anxiety |>
  dplyr::select(id = subject, group = treatment, time = measurement, outcome = anxiety) |>
  dplyr::mutate(id = as.character(id))

manual_groups <- imported_data |>
  dplyr::select(id, group) |>
  dplyr::distinct(id, group)


# Get n of measurements and first (pre) and last (post) measurement
wide_data <- imported_data |>
  stats::na.omit() |>
  dplyr::summarise(
    n = dplyr::n(),
    pre = dplyr::first(outcome),
    post = dplyr::last(outcome),
    .by = id
  )

cutoff_data <- wide_data |>
  dplyr::left_join(manual_groups, dplyr::join_by("id")) |>
  dplyr::filter(n >= 3) |>
  dplyr::relocate(group, .after = id)


# Only use those participants with more than one measurement
prepped_data <- imported_data |>
  dplyr::filter(id %in% cutoff_data[["id"]])


# Determine min and max of measurements (needed for plotting)
min_measurement <- min(prepped_data[["time"]])
max_measurement <- max(prepped_data[["time"]])



# Tests
test_that("data is prepared correctly", {
  prepped_list_claus <- .prep_data(claus_2020, id, time, bdi, pre = 1, post = 4, method = "JT")
  prepped_list_jacobson <- .prep_data(jacobson_1989, subject, time, gds, pre = "pre", method = "JT")

  jacobson_factor <- jacobson_1989 |>
    dplyr::mutate(
      time = factor(time, levels = c("pre", "post"))
    )

  prepped_factor_data_list <- .prep_data(jacobson_factor, subject, time, pre = "pre", gds, method = "JT")

  # Claus data
  expect_equal(prepped_list_claus[["data"]], correct_data_claus)

  # Jacobson data
  expect_equal(prepped_list_jacobson[["data"]], correct_data_jacobson)

  # Factor data
  expect_snapshot(.prep_data(jacobson_factor, subject, time, pre = "pre", gds, method = "JT"))
  expect_equal(prepped_factor_data_list[["wide"]], factor_wide_data)
})

test_that("data is prepared correctly for HLM method", {
  prepped_list <- .prep_data(anxiety, subject, measurement, anxiety, group = treatment, method = "HLM")

  expect_equal(prepped_list[["wide"]], wide_data)
  expect_equal(prepped_list[["groups"]], manual_groups)
  expect_equal(prepped_list[["data"]], cutoff_data)
})
