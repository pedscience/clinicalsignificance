rci_data_gln <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(2, 2, 2, 2, 2, 2, 2))
m_pre <- 5
sd_pre <- 7
reliability <- 0.80
se_prediction <- .calc_se_prediction(sd_pre, reliability)

rcis_gln <- rci_data_gln |>
  mutate(
    pre_adj = reliability * (pre - m_pre),
    post_adj = post - m_pre,
    change_adj = post_adj - pre_adj,
    rci = change_adj / se_prediction
  ) |>
  pull(rci)


test_that("RCI for GLN method is calculated correctly", {
  expect_type(.calc_rci_gln(rci_data_gln, m_pre ,sd_pre, reliability), "list")
  expect_equal(.calc_rci_gln(rci_data_gln, m_pre ,sd_pre, reliability)[["data"]][["rci"]], rcis_gln)
})
