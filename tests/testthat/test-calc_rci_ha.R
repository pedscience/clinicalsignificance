rci_data_ha <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(3, 2, 2, 2, 2, 2, 1))
m_pre <- 10
sd_pre <- 5
m_post <- 7
sd_post <- 6
reliability <- 0.80

se_measurement <- .calc_se_measurement(sd_pre, reliability)
r_xx_1 <- .calc_reliability_ha(sd_pre, se_measurement)
r_xx_2 <- .calc_reliability_ha(sd_post, se_measurement)
cor_pre_post <- cor(rci_data_ha[["pre"]], rci_data_ha[["post"]])

nominator <- (sd_pre^2 * r_xx_1 + sd_post^2 * r_xx_2 - 2 * sd_pre * sd_post * cor_pre_post)
denominator <- (sd_pre^2 + sd_post^2 - 2 * sd_pre * sd_post * cor_pre_post)

r_dd <- nominator / denominator

manual_rci <- rci_data_ha |>
  mutate(
    rci = ((post - pre) * r_dd + (m_post - m_pre) * (1 - r_dd)) / (sqrt(r_dd) * sqrt(2 * se_measurement^2))
  ) %>%
  pull(rci)


test_that("RCI for HA method is calculated correctly", {
  expect_type(.calc_rci_ha(rci_data_ha, m_pre, sd_pre, m_post, sd_post, reliability), "list")
  expect_equal(.calc_rci_ha(rci_data_ha, m_pre, sd_pre, m_post, sd_post, reliability)[["r_dd"]], r_dd)
  expect_equal(.calc_rci_ha(rci_data_ha, m_pre, sd_pre, m_post, sd_post, reliability)[["se_measurement"]], se_measurement)
  expect_equal(.calc_rci_ha(rci_data_ha, m_pre, sd_pre, m_post, sd_post, reliability)[["data"]][["rci"]], manual_rci)
})
