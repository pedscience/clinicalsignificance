clinisig_object <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, reliability_post = 0.90, method = "NK")

# Get values for calculation
m_pre <- get_cutoff_descriptives(clinisig_object)[["m_clinical"]]
sd_pre <- get_cutoff_descriptives(clinisig_object)[["sd_clinical"]]
reliability_pre <- get_reliability(clinisig_object)[[1]]
reliability_post <- get_reliability(clinisig_object)[[2]]

manual_calculation <- tibble(
  pre = c(0, 100),
  ymin = -1.96 * sqrt((reliability_pre^2 * sd_pre^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post))) + (reliability_pre * (pre - m_pre) + m_pre),
  ymax = 1.96 * sqrt((reliability_pre^2 * sd_pre^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post))) + (reliability_pre * (pre - m_pre) + m_pre)
)

test_that("multiplication works", {
  rci_data <- .generate_rci_data_nk(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})
