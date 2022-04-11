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


test_that("Recovered categories are calculated correctly", {
  expect_snapshot(.calc_recovered(ids, cutoff_data, rci_data))
  expect_snapshot(.calc_recovered_ha(ids, cutoff_data, rci_data))
})
