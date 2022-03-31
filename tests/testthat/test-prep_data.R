# Prepare correct data transformation
correct_data_claus <- claus_2020 %>%
  select(id, time, bdi) %>%
  filter(time %in% c(1, 4)) %>%
  pivot_wider(
    names_from = time,
    values_from = bdi,
    names_prefix = "t_"
  ) %>%
  rename(pre = t_1, post = t_4) %>%
  mutate(change = post - pre) %>%
  na.omit()


correct_data_jacobson <- jacobson_1989 %>%
  select(id = subject, time, gds) %>%
  pivot_wider(
    names_from = time,
    values_from = gds
  ) %>%
  mutate(change = post - pre) %>%
  na.omit()


test_that("data is prepared correctly", {
  prepped_list_claus <- claus_2020 %>%
    .prep_data(id, time, bdi, pre = 1, post = 4)

  prepped_list_jacobson <- jacobson_1989 %>%
    .prep_data(subject, time, gds, pre = "pre")

  # Claus data
  expect_type(prepped_list_claus, "list")
  expect_equal(prepped_list_claus[["data"]], correct_data_claus)
  expect_error(clinical_significance(claus_2020, id, time, bdi, reliability = 0.80))

  # Jacobson data
  expect_type(prepped_list_jacobson, "list")
  expect_equal(prepped_list_jacobson[["data"]], correct_data_jacobson)
  expect_message(clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.80))
})
