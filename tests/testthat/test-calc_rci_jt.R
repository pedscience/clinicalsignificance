rci_data_jt <- tibble(id = 1:5, change = c(-8, -4, 0, 4, 8))
sd_pre <- 5
reliability = 0.80
se_measurement <- .calc_se_measurement(sd_pre, reliability)
s_diff <- .calc_s_diff(se_measurement)

manual_rcis <- rci_data_jt |>
  mutate(
    rci = change / s_diff
  ) |>
  pull(rci)

test_that("RCI for JT method is calculated correctly", {
  expect_type(.calc_rci_jt(rci_data_jt, sd_pre, reliability), "list")
  expect_equal(.calc_rci_jt(rci_data_jt, sd_pre, reliability)[["s_diff"]], s_diff)
  expect_equal(.calc_rci_jt(rci_data_jt, sd_pre, reliability)[["data"]][["rci"]], manual_rcis)
})
