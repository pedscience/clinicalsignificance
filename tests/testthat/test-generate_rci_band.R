# RCI JT ------------------------------------------------------------------
clinisig_object <- cs_distribution(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.80)

# Variable for calculation
s_diff <- clinisig_object[["rci_results"]][["s_diff"]]

manual_results <- tibble(
  pre = c(0, 100),
  ymin = pre - qnorm(1 - 0.05/2) * s_diff,
  ymax = pre + qnorm(1 - 0.05/2) * s_diff
)


test_that("RCI data for JT method plotting is calculated correctly", {
  rci_data <- generate_rci_band(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_results)
})



# RCI GLN -----------------------------------------------------------------
clinisig_object <- cs_distribution(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, rci_method = "GLN")


# Get values for calculation
s_prediction <- clinisig_object[["rci_results"]][[1]]
reliability <- cs_get_reliability(clinisig_object)[[1]]
m_pre <- mean(cs_get_data(clinisig_object)[["pre"]])


# Manual calculation
manual_calculation <- tibble(
  pre = c(0, 100),
  ymin = reliability * pre - reliability * m_pre + m_pre + (-qnorm(1 - 0.05/2) * s_prediction),
  ymax = reliability * pre - reliability * m_pre + m_pre + (qnorm(1 - 0.05/2) * s_prediction)
)

test_that("RCI data for GLN method plotting is calculated correctly", {
  rci_data <- generate_rci_band(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})



# RCI HLL -----------------------------------------------------------------
clinisig_object <- cs_distribution(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, rci_method = "HLL")


# Get values for calculation
s_prediction <- clinisig_object[["rci_results"]][[1]]
m_post <- clinisig_object[["rci_results"]][["m_post"]]
reliability <- cs_get_reliability(clinisig_object)[[1]]
m_pre <- mean(cs_get_data(clinisig_object)[["pre"]])


# Manual calculation
manual_calculation <- tibble(
  pre = c(0, 100),
  ymin = -qnorm(1 - 0.05/2) * s_prediction + m_post + reliability * .data$pre - reliability * m_pre,
  ymax = qnorm(1 - 0.05/2) * s_prediction + m_post + reliability * .data$pre - reliability * m_pre
)


test_that("RCI data for HLL method plotting is calculated correctly", {
  rci_data <- generate_rci_band(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})
