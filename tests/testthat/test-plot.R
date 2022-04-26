library(vdiffr)
test_that("plots are correct", {
  plot_jt <- plot(clinical_significance(anxiety, subject, measurement, anxiety, pre = 0, post = 3, reliability = 0.80))
  plot_gln <- plot(clinical_significance(anxiety, subject, measurement, anxiety, pre = 0, post = 3, reliability = 0.80, method = "GLN"))
  plot_hll <- plot(clinical_significance(anxiety, subject, measurement, anxiety, pre = 0, post = 3, reliability = 0.80, method = "HLL"))
  plot_en <- plot(clinical_significance(anxiety, subject, measurement, anxiety, pre = 0, post = 3, reliability = 0.80, method = "EN"))
  plot_nk <- plot(clinical_significance(anxiety, subject, measurement, anxiety, pre = 0, post = 3, reliability = 0.80, reliability_post = 0.90, method = "NK"))
  plot_ha <- plot(clinical_significance(anxiety, subject, measurement, anxiety, pre = 0, post = 3, reliability = 0.80, method = "HA"))

  expect_doppelganger("base plot jt", plot_jt)
  expect_doppelganger("base plot gln", plot_gln)
  expect_doppelganger("base plot hll", plot_hll)
  expect_doppelganger("base plot en", plot_en)
  expect_doppelganger("base plot nk", plot_nk)
  expect_doppelganger("base plot ha", plot_ha)
})


test_that("grouping in plots is correct", {
  group_plot <- plot(clinical_significance(anxiety, subject, measurement, anxiety, pre = 0, post = 3, group = treatment, reliability = 0.80))

  expect_doppelganger("base plot jt with grouping", group_plot)
})


test_that("HLM plots are correct", {
  plot_trajectory <- plot(clinical_significance(anxiety, subject, measurement, anxiety, method = "HLM"), which = "trajectory")
  plot_slope <- plot(clinical_significance(anxiety, subject, measurement, anxiety, method = "HLM"), which = "slope")
  plot_trajectory_grouped <- plot(clinical_significance(anxiety, subject, measurement, anxiety, group = treatment, method = "HLM"), which = "trajectory")
  plot_slope_grouped <- plot(clinical_significance(anxiety, subject, measurement, anxiety, group = treatment, method = "HLM"), which = "slope")

  expect_doppelganger("trajectories ungrouped", plot_trajectory)
  expect_doppelganger("EB slopes ungrouped", plot_slope)
  expect_doppelganger("trajectories grouped", plot_trajectory_grouped)
  expect_doppelganger("EB slopes grouped", plot_slope_grouped)
})


test_that("Plot method throws expected errors", {
  expect_snapshot_error(plot(clinical_significance(anxiety, subject, measurement, anxiety, pre = 0, post = 3, reliability = 0.80), include_cutoff_band = TRUE))
})
