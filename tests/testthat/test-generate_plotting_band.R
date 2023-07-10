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
  rci_data <- generate_plotting_band(clinisig_object)

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
  rci_data <- generate_plotting_band(clinisig_object)

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
  rci_data <- generate_plotting_band(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})




# RCI EN -----------------------------------------------------------------
clinisig_object <- cs_distribution(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, rci_method = "EN")


# Get values for calculation
se_measurement <- clinisig_object[["rci_results"]][["se_measurement"]]
reliability <- cs_get_reliability(clinisig_object)[[1]]
m_pre <- mean(cs_get_data(clinisig_object)[["pre"]])


# Manual calculation
manual_calculation <- tibble(
  pre = c(0, 100),
  pre_true = reliability * (pre - m_pre) + m_pre,
  ymin = pre_true - qnorm(1 - 0.05/2) * se_measurement,
  ymax = pre_true + qnorm(1 - 0.05/2) * se_measurement
)

test_that("RCI data for EN method plotting is calculated correctly", {
  rci_data <- generate_plotting_band(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})




# RCI NK -----------------------------------------------------------------
clinisig_object <- cs_distribution(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, reliability_post = 0.75, rci_method = "NK")


# Get values for calculation
m_pre <- mean(cs_get_data(clinisig_object)[["pre"]])
sd_pre <- sd(cs_get_data(clinisig_object)[["pre"]])
reliability_pre <- cs_get_reliability(clinisig_object)[[1]]
reliability_post <- cs_get_reliability(clinisig_object)[[2]]


# Manual calculation
manual_calculation <- tibble(
  pre = c(0, 100),
  ymin = -qnorm(1 - 0.05/2) * sqrt((reliability_pre^2 * sd_pre^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post))) + (reliability_pre * (pre - m_pre) + m_pre),
  ymax = qnorm(1 - 0.05/2) * sqrt((reliability_pre^2 * sd_pre^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post))) + (reliability_pre * (pre - m_pre) + m_pre)
)

test_that("multiplication works", {
  rci_data <- generate_plotting_band(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})




# RCI HA -----------------------------------------------------------------
clinisig_object <- cs_distribution(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, rci_method = "HA")


# Get values for calculation
r_dd <- clinisig_object[["rci_results"]][["r_dd"]]
se_measurement <- clinisig_object[["rci_results"]][["se_measurement"]]
m_pre <- mean(cs_get_data(clinisig_object)[["pre"]])
m_post <- mean(cs_get_data(clinisig_object)[["post"]])


# Manual calculation
# RCI
manual_calculation <- tibble(
  pre = c(0, 100),
  ymin = (-qnorm(1 - 0.05) * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd,
  ymax = (qnorm(1 - 0.05) * sqrt(r_dd) * sqrt(2 * se_measurement^2) - (m_post - m_pre) * (1 - r_dd) + pre * r_dd) / r_dd
)

test_that("RCI data for HA method plotting is calculated correctly", {
  rci_data <- generate_plotting_band(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_calculation)
})
