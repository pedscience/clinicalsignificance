# Helpers -----------------------------------------------------------------
sd <- 5
reliability <- 0.80
se_measurement <- sd * sqrt(1 - reliability)
se_prediction <- sd * sqrt(1 - reliability^2)
ha_reliability <- (sd^2 - se_measurement^2) / sd^2

test_that("SE of measurement is calculated correctly", {
 expect_equal(.calc_se_measurement(sd, reliability), se_measurement)
})


test_that("S_diff is calculated correctly", {
  expect_equal(.calc_s_diff(se_measurement), sqrt(2 * se_measurement^2))
})


test_that("SE of prediction is calculated correctly", {
  expect_equal(.calc_se_prediction(sd, reliability), se_prediction)
})


test_that("Reliability in HA method is calculated correctly", {
  expect_equal(.calc_reliability_ha(sd, se_measurement), ha_reliability)
})




# RCI Categories ----------------------------------------------------------
rci_data <- tibble::tibble(id = 1:5, rci = c(-2, -0.5, 0, 0.5, 2))

rci_categroies_higher <- rci_data |>
  dplyr::mutate(
    improved = rci > 1.96,
    deteriorated = rci < -1.96,
    unchanged = !improved & !deteriorated
  )


rci_categroies_lower <- rci_data |>
  dplyr::mutate(
    improved = rci < -1.96,
    deteriorated = rci > 1.96,
    unchanged = !improved & !deteriorated
  )


test_that("RCI categories are calculated correctly", {
  expect_equal(.calc_improvement(rci_data, direction = 1), rci_categroies_higher)
  expect_equal(.calc_improvement(rci_data, direction = -1), rci_categroies_lower)
})
