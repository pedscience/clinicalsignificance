clinisig_object <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, method = "EN")


# Get values for calculation
se_measurement <- clinisig_object$rci$se_measurement
reliability <- get_reliability(clinisig_object)[[1]]
m_pre <- get_cutoff_descriptives(clinisig_object)[["m_clinical"]]


# Manual calculation
manual_calculation <- tibble(
  pre = c(0, 100),
  pre_true = reliability * (pre - m_pre) + m_pre,
  ymin = pre_true - 2 * se_measurement,
  ymax = pre_true + 2 * se_measurement
)

test_that("RCI data for EN method plotting is calculated correctly", {
  rci_data <- .generate_rci_data_en(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})
