test_that("clinical_significance throws expected errors", {
  # ID missing
  expect_snapshot_error(clinical_significance(claus_2020))

  # Time column missing
  expect_snapshot_error(clinical_significance(claus_2020, id))

  # Missing outcome
  expect_snapshot_error(clinical_significance(claus_2020, id, time))

  # Missing reliability when method != HLM
  expect_snapshot_error(clinical_significance(claus_2020, id, time, bdi))

  # More than two measurements in data and method != HLM
  expect_snapshot_error(clinical_significance(claus_2020, id, time, bdi, reliability = 0.80))

  # Expect message when method == NK and second reliability is not set
  expect_snapshot_warning(clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.80, method = "NK"), class = "message")

  # Functional population descriptives missing
  expect_snapshot_error(clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.80, type = "c"))

  # Functional population info given, but type was not b or c (which use
  # functional population info)
  expect_snapshot_warning(clinical_significance(claus_2020, id, time, bdi, pre = 1, post = 4, reliability = 0.80, m_functional = 8, sd_functional = 8), class = "message")

  # Specified post reliability but did not choose NK method
  expect_snapshot_warning(clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.80, reliability_post = 0.90, pre = "pre", method = "JT"), class = "message")
})


test_that("clinical_significance output is correct", {
  expect_snapshot(clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.80, pre = "pre", method = "JT"))
  expect_snapshot(clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.80, pre = "pre", method = "GLN"))
  expect_snapshot(clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.80, pre = "pre", method = "HLL"))
  expect_snapshot(clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.80, pre = "pre", method = "EN"))
  expect_snapshot(clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.80, reliability_post = 0.80, pre = "pre", method = "NK"))
  expect_snapshot(clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.80, pre = "pre", method = "HA"))
  expect_snapshot(clinical_significance(anxiety, subject, measurement, anxiety, method = "HLM"))

  # Test correct grouping alignment
  expect_snapshot(clinical_significance(claus_2020, id, time, bdi, group = treatment, reliability = 0.80, pre = 1, post = 4))
})


test_that("summary is correct", {
  # JT method without and with functional population and grouping
  expect_snapshot(summary(clinical_significance(claus_2020, id, time, bdi, reliability = 0.80, pre = 1, post = 4, method = "JT")))
  expect_snapshot(summary(clinical_significance(claus_2020, id, time, bdi, reliability = 0.80, pre = 1, post = 4, m_functional = 8, sd_functional = 8, type = "c", method = "JT")))
  expect_snapshot(summary(clinical_significance(claus_2020, id, time, bdi, pre = 1, post = 4, reliability = 0.80, group = treatment)))

  # HA method (contains two summary tables)
  expect_snapshot(summary(clinical_significance(claus_2020, id, time, bdi, reliability = 0.80, pre = 1, post = 4, m_functional = 8, sd_functional = 8, type = "c", method = "HA")))

  # HLM (reliability is not specified here)
  expect_snapshot(summary(clinical_significance(anxiety, subject, measurement, anxiety, method = "HLM")))
})
