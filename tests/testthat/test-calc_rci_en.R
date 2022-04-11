# Data and descriptives
rci_data_en <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(2, 2, 2, 2, 2, 2, 2))
m_pre <- 10
sd_pre <- 5
reliability <- 0.80
se_measurement <- .calc_se_measurement(sd_pre, reliability)

# Calc confidence bounds
rci_results_en <- rci_data_en %>%
  mutate(
    pre_true = reliability * (pre - m_pre) + m_pre,
    lower = pre_true - 2 * se_measurement,
    upper = pre_true + 2 * se_measurement
  )

# Determine change for higher = better outcomes
rci_categories_en_higher <- rci_results_en %>%
  mutate(
    improved = post > upper,
    deteriorated = post < lower,
    unchanged = !improved & !deteriorated
  )

# Determine change for lower = better outcomes
rci_categories_en_lower <- rci_results_en %>%
  mutate(
    improved = post < lower,
    deteriorated = post > upper,
    unchanged = !improved & !deteriorated
  )

# Extract results for comparison
manual_pre_true <- rci_results_en[["pre_true"]]
manual_lower <- rci_results_en[["lower"]]
manual_upper <- rci_results_en[["upper"]]
improved_higher <- rci_categories_en_higher[["improved"]]
deteriorated_higher <- rci_categories_en_higher[["deteriorated"]]
unchanged_higher <- rci_categories_en_higher[["unchanged"]]
improved_lower <- rci_categories_en_lower[["improved"]]
deteriorated_lower <- rci_categories_en_lower[["deteriorated"]]
unchanged_lower <- rci_categories_en_lower[["unchanged"]]


test_that("RCI for EN method is calculated correctly", {
  results_higher <- .calc_rci_en(rci_data_en, m_pre, sd_pre, reliability, direction = 1)
  results_lower <- .calc_rci_en(rci_data_en, m_pre, sd_pre, reliability, direction = -1)

  expect_type(results_higher, "list")
  expect_equal(results_higher[["se_measurement"]], se_measurement)
  expect_equal(results_higher[["data"]][["pre_true"]], manual_pre_true)
  expect_equal(results_higher[["data"]][["lower"]], manual_lower)
  expect_equal(results_higher[["data"]][["upper"]], manual_upper)
  expect_equal(results_higher[["data"]][["improved"]], improved_higher)
  expect_equal(results_higher[["data"]][["deteriorated"]], deteriorated_higher)
  expect_equal(results_higher[["data"]][["unchanged"]], unchanged_higher)
  expect_equal(results_lower[["data"]][["improved"]], improved_lower)
  expect_equal(results_lower[["data"]][["deteriorated"]], deteriorated_lower)
  expect_equal(results_lower[["data"]][["unchanged"]], unchanged_lower)
})
