# Data and descriptives
rci_data_en <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(2, 2, 2, 2, 2, 2, 2))
m_pre <- 10
sd_pre <- 5
reliability <- 0.80
se_measurement <- .calc_se_measurement(sd_pre, reliability)

# Calculate the RCI according to reformulation of Speer
manual_rci_data <- rci_data_en %>%
  mutate(
    pre_true = reliability * (pre - m_pre) + m_pre,
    change_adj = post - pre_true,
    rci = change_adj / se_measurement
  )

# Calculate categories
data_rci_categories <- .calc_improvement(
  data = manual_rci_data,
  rci_cutoff = 2,
  direction = -1
)


test_that("RCI for EN method is calculated correctly", {
  expect_type(.calc_rci_en(rci_data_en, m_pre, sd_pre, reliability, -1), "list")
  expect_equal(.calc_rci_en(rci_data_en, m_pre ,sd_pre, reliability, -1)[["data"]][["rci"]], manual_rci_data$rci)
})
