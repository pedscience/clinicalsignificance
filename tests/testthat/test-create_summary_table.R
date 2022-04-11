# For ungrouped data
cutoff_data <- tibble(
  id = 1:5,
  clinical_pre = c(TRUE, FALSE, FALSE, FALSE, FALSE),
  functional_post = c(TRUE, FALSE, FALSE, TRUE, FALSE)
)


rci_data <- tibble(
  id = 1:5,
  improved = c(TRUE, TRUE, FALSE, FALSE, FALSE),
  deteriorated = c(FALSE, FALSE, FALSE, TRUE, TRUE),
  unchanged = c(FALSE, FALSE, TRUE, FALSE, FALSE)
)

ids <- tibble(
  id = 1:5,
  rci = "Not needed",
  cs_indiv = "Not needed"
)

# For grouped data
cutoff_data_grouped <- tibble(
  id = 1:7,
  clinical_pre = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  functional_post = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
)


rci_data_grouped <- tibble(
  id = 1:7,
  improved = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  deteriorated = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
  unchanged = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
)

ids_grouped <- tibble(
  id = 1:7,
  group = c(1, 1, 1, 1, 1, 2, 2),
  rci = "Not needed",
  cs_indiv = "Not needed"
)

recovered_data <- .calc_recovered(ids, cutoff_data, rci_data)
recovered_data_grouped <- .calc_recovered(ids_grouped, cutoff_data_grouped, rci_data_grouped)


test_that("The summary table is created correctly", {
  expect_equal(.create_summary_table(recovered_data)[["n"]], rep(1, each = 5))
  expect_equal(.create_summary_table(recovered_data_grouped)[["n"]], c(1, 1, 1, 1, 1, 0, 0, 0, 1, 1))
})
