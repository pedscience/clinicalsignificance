cs_results_anchor <- claus_2020 |>
  cs_anchor(id, time, bdi, pre = 1, post = 4, mid_improvement = 9)

cs_results_distribution <- claus_2020 |>
  cs_distribution(id, time, bdi, pre = 1, post = 4, reliability = 0.80)

cs_results_statistical <- claus_2020 |>
  cs_statistical(id, time, bdi, pre = 1, post = 4, m_functional = 8, sd_functional = 8, cutoff_type = "c")

cs_results_combined <- claus_2020 |>
  cs_combined(id, time, bdi, pre = 1, post = 4, reliability = 0.80, m_functional = 8, sd_functional = 8, cutoff_type = "c")

cs_results_percentage <- claus_2020 |>
  cs_percentage(id, time, bdi, pre = 1, post = 4, pct_improvement = 0.3)


test_that("cs_get_n retrieves correct numbers", {
  expect_equal(cs_get_n(cs_results_anchor)[["n_original"]], 43)
  expect_equal(cs_get_n(cs_results_anchor)[["n_used"]], 40)
  expect_equal(cs_get_n(cs_results_anchor)[["percent_used"]], 40/43)

  expect_equal(cs_get_n(cs_results_anchor, "original")[[1]], 43)
  expect_equal(cs_get_n(cs_results_anchor, "used")[[1]], 40)
})


test_that("n can be extracted from every approach", {
  expect_snapshot(cs_get_n(cs_results_anchor))
  expect_snapshot(cs_get_n(cs_results_distribution))
  expect_snapshot(cs_get_n(cs_results_statistical))
  expect_snapshot(cs_get_n(cs_results_combined))
  expect_snapshot(cs_get_n(cs_results_percentage))
})
