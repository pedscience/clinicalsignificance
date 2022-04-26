# Names are only to be expected for methods != HA.
test_that("extracted cutoff info is correct", {
  function_cutoff <- get_cutoff(clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.80, m_functional = 60, sd_functional = 7.5, type = "c"))[["value"]][[1]]
  cutoff_tibble <- get_cutoff(clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.80, m_functional = 60, sd_functional = 7.5, type = "c"), with_descriptives = TRUE)
  cutoff_tibble_ha <- get_cutoff(clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.80, m_functional = 60, sd_functional = 7.5, type = "c", method = "HA"), with_descriptives = TRUE)
  expected_names <- c("m_clinical", "sd_clinical", "m_functional", "sd_functional", "type", "value")
  expected_names_ha <- c("m_clinical", "sd_clinical", "m_functional", "sd_functional", "reliability_clinical", "reliability_functional", "type", "value")

  expect_equal(function_cutoff, 65.047305)
  expect_s3_class(cutoff_tibble, "tbl_df")
  expect_equal(names(cutoff_tibble), expected_names)
  expect_equal(names(cutoff_tibble_ha), expected_names_ha)
})
