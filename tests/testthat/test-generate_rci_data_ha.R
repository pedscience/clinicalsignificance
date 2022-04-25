clinisig_object <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, method = "HA")

# Get values for calculation
r_dd <- clinisig_object[["rci"]][[1]]
se_measurement <- clinisig_object[["rci"]][["se_measurement"]]
aug_data <- get_augmented_data(clinisig_object)
m_pre <- mean(aug_data[["pre"]])
m_post <- mean(aug_data[["post"]])
rel_post <- clinisig_object[["cutoff"]][["reliability_post"]]
cutoff <- clinisig_object[["cutoff"]][["info"]][["value"]]


# Manual calculation
# RCI
manual_calculation <- tibble(
  pre = c(0, 100),
  ymin = (-1.65 * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd,
  ymax = (1.65 * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd
)

# Cutoff band
manual_calculation_cutoff <- tibble(
  pre = c(0, 100),
  ymin = (-1.65 * sqrt(rel_post) * se_measurement - m_post + m_post * rel_post + cutoff) / rel_post,
  ymax = (1.65 * sqrt(rel_post) * se_measurement - m_post + m_post * rel_post + cutoff) / rel_post
)


# Tests
test_that("RCI data for HA method plotting is calculated correctly", {
  rci_data <- .generate_rci_data_ha(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})


test_that("Cutoff band data for HA method plotting is calculated correctly", {
  cut_data <- .generate_true_cut_data(clinisig_object)

  expect_s3_class(cut_data, "tbl_df")
  expect_equal(cut_data, manual_calculation_cutoff)
})
