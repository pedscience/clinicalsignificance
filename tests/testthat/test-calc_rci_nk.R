rci_data_nk <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(2, 2, 2, 2, 2, 2, 2))
m_pre <- 10
sd_pre <- 5
reliability <- 0.80
reliability_post <- 0.80
denominator <- sqrt((reliability^2 * sd_pre ^2 * (1 - reliability)) + (sd_pre^2 * (1 - reliability_post)))


manual_rci <- rci_data_nk %>%
  mutate(
    pre_adj = reliability * (pre - m_pre) + m_pre,
    change_adj = post - pre_adj,
    rci = change_adj / denominator
  ) %>%
  pull(rci)


test_that("RCI for method NK is calculated correctly", {
  expect_type(.calc_rci_nk(rci_data_nk, m_pre, sd_pre, reliability, reliability_post), "list")
  expect_equal(.calc_rci_nk(rci_data_nk, m_pre, sd_pre, reliability, reliability_post)[["data"]][["rci"]], manual_rci)
})
