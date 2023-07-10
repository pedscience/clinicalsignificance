claus_jt <- claus_2020 |>
  cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.80)

claus_en <- claus_2020 |>
  cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "EN")

claus_ha <- claus_2020 |>
  cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "HA")

claus_hlm <- claus_2020 |>
  cs_distribution(id, time, hamd, rci_method = "HLM")


test_that("Plots are created correctly", {
  vdiffr::expect_doppelganger("JT Plot", plot(claus_jt))
  vdiffr::expect_doppelganger("EN Plot", plot(claus_en))
  vdiffr::expect_doppelganger("HA Plot", plot(claus_ha))
  vdiffr::expect_doppelganger("HLM Plot", plot(claus_hlm))
})
