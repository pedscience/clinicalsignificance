rci_data_hll <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(3, 2, 2, 2, 2, 2, 1))
m_pre <- 10
sd_pre <- 5
m_post <- 7
reliability <- 0.80
se_prediction <- .calc_se_prediction(sd_pre, reliability)


# Calculate RCI
manual_rci <- rci_data_hll |>
  mutate(
    pre_adj = reliability * (pre - m_pre),
    post_adj = post - m_post,
    change_adj = post_adj - pre_adj,
    rci = change_adj / se_prediction
  ) |>
  pull(rci)


test_that("RCI for HLL method is calculated correctly", {
  expect_type(.calc_rci_hll(rci_data_hll, m_pre, sd_pre, m_post, reliability), "list")
  expect_equal(.calc_rci_hll(rci_data_hll, m_pre, sd_pre, m_post, reliability)[["se_prediction"]], se_prediction)
  expect_equal(.calc_rci_hll(rci_data_hll, m_pre, sd_pre, m_post, reliability)[["m_post"]], m_post)
  expect_equal(.calc_rci_hll(rci_data_hll, m_pre, sd_pre, m_post, reliability)[["data"]][["rci"]], manual_rci)
})
