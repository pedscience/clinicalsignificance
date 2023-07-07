library(dplyr)
library(tidyr)

# Prepare correct data transformation
correct_data_claus <- claus_2020 |>
  select(id, time, bdi) |>
  filter(time %in% c(1, 4)) |>
  pivot_wider(
    names_from = time,
    values_from = bdi,
    names_prefix = "t_"
  ) |>
  rename(pre = t_1, post = t_4) |>
  mutate(change = post - pre) |>
  na.omit()


correct_data_jacobson <- jacobson_1989 |>
  select(id = subject, time, gds) |>
  pivot_wider(
    names_from = time,
    values_from = gds
  ) |>
  mutate(change = post - pre) |>
  na.omit()


# Factor sorting test
factor_wide_data <- jacobson_1989 |>
  mutate(
    time = factor(time, levels = c("pre", "post"))
  ) |>
  .prep_data(subject, time, gds, method = "JT") |>
  purrr::pluck("wide")



# HLM Data
imported_data <- anxiety |>
  select(id = subject, group = treatment, time = measurement, outcome = anxiety) |>
  mutate(id = as.character(id))

manual_groups <- imported_data |>
  select(id, group) |>
  distinct(id, group)


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

cutoff_data <- wide_data |>
  left_join(manual_groups, by = "id") |>
  filter(n >= 3) |>
  relocate(group, .after = id)


# Only use those participants with more than one measurement
prepped_data <- imported_data |>
  filter(id %in% cutoff_data[["id"]])


# Determine min and max of measurements (needed for plotting)
min_measurement <- min(prepped_data[["time"]])
max_measurement <- max(prepped_data[["time"]])



# Tests
test_that("data is prepared correctly", {
  prepped_list_claus <- .prep_data(claus_2020, id, time, bdi, pre = 1, post = 4, method = "JT")
  prepped_list_jacobson <- .prep_data(jacobson_1989, subject, time, gds, pre = "pre", method = "JT")

  jacobson_factor <- jacobson_1989 |>
    mutate(
      time = factor(time, levels = c("pre", "post"))
    )

  prepped_factor_data_list <- .prep_data(jacobson_factor, subject, time, gds, method = "JT")

  # Claus data
  expect_s3_class(prepped_list_claus, "cs_jt")
  expect_equal(prepped_list_claus[["data"]], correct_data_claus)

  # Jacobson data
  expect_s3_class(prepped_list_jacobson, "cs_jt")
  expect_equal(prepped_list_jacobson[["data"]], correct_data_jacobson)

  # Factor data
  expect_snapshot(.prep_data(jacobson_factor, subject, time, gds, method = "JT"))
  expect_equal(prepped_factor_data_list[["wide"]], factor_wide_data)
})

test_that("data is prepared correctly for HLM method", {
  prepped_list <- .prep_data(anxiety, subject, measurement, anxiety, group = treatment, method = "HLM")

  expect_s3_class(prepped_list, "cs_hlm")
  expect_equal(prepped_list[["wide"]], wide_data)
  expect_equal(prepped_list[["groups"]], manual_groups)
  expect_equal(prepped_list[["data"]], cutoff_data)
})
