clinisig_object <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, method = "GLN")


# Get values for calculation
s_prediction <- clinisig_object[["rci"]][[1]]
reliability <- get_reliability(clinisig_object)[[1]]
m_pre <- get_cutoff_descriptives(clinisig_object)[["m_clinical"]]


# Manual calculation
manual_calculation <- tibble(
  pre = c(0, 100),
  ymin = reliability * pre - reliability * m_pre + m_pre + (-qnorm(1 - 0.05/2) * s_prediction),
  ymax = reliability * pre - reliability * m_pre + m_pre + (qnorm(1 - 0.05/2) * s_prediction)
)

test_that("RCI data for GLN method plotting is calculated correctly", {
  rci_data <- .generate_rci_data_gln(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})
