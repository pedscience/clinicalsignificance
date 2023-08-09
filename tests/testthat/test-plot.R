claus_jt <- claus_2020 |>
  cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.80)

claus_en <- claus_2020 |>
  cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "EN")

claus_ha <- claus_2020 |>
  cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "HA")

claus_hlm <- claus_2020 |>
  cs_distribution(id, time, hamd, rci_method = "HLM")

claus_jt_statistical <- claus_2020 |>
  cs_statistical(id, time, hamd, pre = 1, post = 4, m_functional = 8, sd_functional = 8, cutoff_type = "c", cutoff_method = "JT")

claus_ha_statistical <- claus_2020 |>
  cs_statistical(id, time, hamd, pre = 1, post = 4, reliability = 0.80, m_functional = 8, sd_functional = 8, cutoff_type = "c", cutoff_method = "HA")

claus_jt_combined <- claus_2020 |>
  cs_combined(id, time, hamd, pre = 1, post = 4, reliability = 0.80, m_functional = 8, sd_functional = 8, cutoff_type = "c")

claus_jt_combined_grouped <- claus_2020 |>
  cs_combined(id, time, hamd, group = treatment, pre = 1, post = 4, reliability = 0.80, m_functional = 8, sd_functional = 8, cutoff_type = "c")

claus_ha_combined <- claus_2020 |>
  cs_combined(id, time, hamd, pre = 1, post = 4, reliability = 0.80, m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "HA")

claus_hlm_combined <- claus_2020 |>
  cs_combined(id, time, hamd, m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "HLM")


claus_pct_impr <- claus_2020 |>
  cs_percentage(id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5)

claus_pct_impr_grouped <- claus_2020 |>
  cs_percentage(id, time, hamd, pre = 1, post = 4, group = treatment, pct_improvement = 0.5)

claus_pct_impr_det <- claus_2020 |>
  cs_percentage(id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5)


test_that("Plots are created correctly", {
  vdiffr::expect_doppelganger("Distribution JT Plot", plot(claus_jt))
  vdiffr::expect_doppelganger("Distribution EN Plot", plot(claus_en))
  vdiffr::expect_doppelganger("Distribution HA Plot", plot(claus_ha))
  vdiffr::expect_doppelganger("Distribution HLM Plot", plot(claus_hlm))
  vdiffr::expect_doppelganger("Statistical JT Plot", plot(claus_jt_statistical))
  vdiffr::expect_doppelganger("Statistical HA Plot", plot(claus_ha_statistical))
  vdiffr::expect_doppelganger("Combined JT Plot", plot(claus_jt_combined))
  vdiffr::expect_doppelganger("Combined HA Plot", plot(claus_ha_combined))
  vdiffr::expect_doppelganger("Combined HLM Plot", plot(claus_hlm_combined))
  vdiffr::expect_doppelganger("Combined JT Plot Grouped", plot(claus_jt_combined_grouped))
  vdiffr::expect_doppelganger("Percentage Plot", plot(claus_pct_impr))
  vdiffr::expect_doppelganger("Percentage Plot Grouped", plot(claus_pct_impr_grouped))
  vdiffr::expect_doppelganger("Percentage Plot with Improvement and Deterioration", plot(claus_pct_impr_det))
})
