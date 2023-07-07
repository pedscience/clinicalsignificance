library(dplyr)
library(tibble)



# RCI JT ------------------------------------------------------------------
rci_data_jt <- tibble(id = 1:5, change = c(-8, -4, 0, 4, 8))
sd_pre <- 5
reliability <- 0.80
se_measurement <- .calc_se_measurement(sd_pre, reliability)
s_diff <- .calc_s_diff(se_measurement)

manual_rcis <- rci_data_jt |>
  mutate(
    rci = change / s_diff
  ) |>
  pull(rci)


# Create an object of respective class
rci_data_list <- list(
  data = rci_data_jt
)
class(rci_data_list) <- c("cs_jt", class(rci_data_list))


test_that("RCI for JT method is calculated correctly", {
  expect_s3_class(calc_rci(data = rci_data_list, sd_pre = sd_pre, reliability = reliability), "cs_distribution")
  expect_equal(calc_rci(data = rci_data_list, sd_pre, reliability)[["s_diff"]], s_diff)
  expect_equal(calc_rci(data = rci_data_list, sd_pre, reliability)[["data"]][["rci"]], manual_rcis)
})




# RCI GLN -----------------------------------------------------------------
rci_data_gln <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(2, 2, 2, 2, 2, 2, 2))
m_pre <- 5
sd_pre <- 7
se_prediction <- .calc_se_prediction(sd_pre, reliability)

rcis_gln <- rci_data_gln |>
  mutate(
    pre_adj = reliability * (pre - m_pre),
    post_adj = post - m_pre,
    change_adj = post_adj - pre_adj,
    rci = change_adj / se_prediction
  ) |>
  pull(rci)


# Create an object of respective class
rci_data_list <- list(
  data = rci_data_gln
)
class(rci_data_list) <- c("cs_gln", class(rci_data_list))


test_that("RCI for GLN method is calculated correctly", {
  expect_s3_class(calc_rci(data = rci_data_list, m_pre = m_pre, sd_pre = sd_pre, reliability = reliability), "cs_distribution")
  expect_equal(calc_rci(data = rci_data_list, m_pre = m_pre, sd_pre = sd_pre, reliability = reliability)[["data"]][["rci"]], rcis_gln)
})




# RCI HLL -----------------------------------------------------------------
rci_data_hll <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(3, 2, 2, 2, 2, 2, 1))
m_pre <- 10
sd_pre <- 5
m_post <- 7
se_prediction <- .calc_se_prediction(sd_pre, reliability)


# Calculate RCI
manual_rci <- rci_data_hll |>
  mutate(
    pre_adj = reliability * (pre - m_pre),
    post_adj = post - m_post,
    change_adj = post_adj - pre_adj,
    rci = change_adj / se_prediction
  ) |>
  pull(rci)


# Create an object of respective class
rci_data_list <- list(
  data = rci_data_hll
)
class(rci_data_list) <- c("cs_hll", class(rci_data_list))


test_that("RCI for HLL method is calculated correctly", {
  expect_type(calc_rci(data = rci_data_list, m_pre = m_pre, sd_pre = sd_pre, m_post = m_post, reliability = reliability), "list")
  expect_equal(calc_rci(data = rci_data_list, m_pre = m_pre, sd_pre = sd_pre, m_post = m_post, reliability = reliability)[["se_prediction"]], se_prediction)
  expect_equal(calc_rci(data = rci_data_list, m_pre = m_pre, sd_pre = sd_pre, m_post = m_post, reliability = reliability)[["m_post"]], m_post)
  expect_equal(calc_rci(data = rci_data_list, m_pre = m_pre, sd_pre = sd_pre, m_post = m_post, reliability = reliability)[["data"]][["rci"]], manual_rci)
})






# RCI EN ------------------------------------------------------------------
rci_data_en <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(2, 2, 2, 2, 2, 2, 2))
m_pre <- 10
sd_pre <- 5
se_measurement <- .calc_se_measurement(sd_pre, reliability)

# Calculate the RCI according to reformulation of Speer
manual_rci_data <- rci_data_en |>
  mutate(
    pre_true = reliability * (pre - m_pre) + m_pre,
    change_adj = post - pre_true,
    rci = change_adj / se_measurement
  )


# Create an object of respective class
rci_data_list <- list(
  data = rci_data_en
)
class(rci_data_list) <- c("cs_en", class(rci_data_list))


test_that("RCI for EN method is calculated correctly", {
  expect_s3_class(calc_rci(data = rci_data_list, m_pre = m_pre, sd_pre = sd_pre, reliability = reliability, -1), "cs_distribution")
  expect_equal(calc_rci(data = rci_data_list, m_pre = m_pre, sd_pre = sd_pre, reliability = reliability, -1)[["data"]][["rci"]], manual_rci_data$rci)
})




# RCI NK ------------------------------------------------------------------
rci_data_nk <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(2, 2, 2, 2, 2, 2, 2))
m_pre <- 10
sd_pre <- 5
reliability_post <- 0.80
denominator <- sqrt((reliability^2 * sd_pre ^2 * (1 - reliability)) + (sd_pre^2 * (1 - reliability_post)))

manual_rci <- rci_data_nk |>
  mutate(
    pre_adj = reliability * (pre - m_pre) + m_pre,
    change_adj = post - pre_adj,
    rci = change_adj / denominator
  ) |>
  pull(rci)


# Create an object of respective class
rci_data_list <- list(
  data = rci_data_nk
)
class(rci_data_list) <- c("cs_nk", class(rci_data_list))


test_that("RCI for method NK is calculated correctly", {
  expect_s3_class(calc_rci(rci_data_list, m_pre = m_pre, sd_pre = sd_pre, reliability = reliability, reliability_post = reliability_post), "cs_distribution")
  expect_equal(calc_rci(rci_data_list, m_pre = m_pre, sd_pre = sd_pre, reliability = reliability, reliability_post = reliability_post)[["data"]][["rci"]], manual_rci)
})




# RCI HA ------------------------------------------------------------------
rci_data_ha <- tibble(id = 1:7, pre = c(30, 20, 10, 8, 6, 4, 2), post = c(3, 2, 2, 2, 2, 2, 1))
m_pre <- 10
sd_pre <- 5
m_post <- 7
sd_post <- 6

se_measurement <- .calc_se_measurement(sd_pre, reliability)
r_xx_1 <- .calc_reliability_ha(sd_pre, se_measurement)
r_xx_2 <- .calc_reliability_ha(sd_post, se_measurement)
cor_pre_post <- cor(rci_data_ha[["pre"]], rci_data_ha[["post"]])

nominator <- (sd_pre^2 * r_xx_1 + sd_post^2 * r_xx_2 - 2 * sd_pre * sd_post * cor_pre_post)
denominator <- (sd_pre^2 + sd_post^2 - 2 * sd_pre * sd_post * cor_pre_post)

r_dd <- nominator / denominator

manual_rci <- rci_data_ha |>
  mutate(
    rci = ((post - pre) * r_dd + (m_post - m_pre) * (1 - r_dd)) / (sqrt(r_dd) * sqrt(2 * se_measurement^2))
  ) |>
  pull(rci)


# Create an object of respective class
rci_data_list <- list(
  data = rci_data_ha
)
class(rci_data_list) <- c("cs_ha", class(rci_data_list))


test_that("RCI for HA method is calculated correctly", {
  expect_s3_class(calc_rci(rci_data_list, m_pre, sd_pre, m_post, sd_post, reliability), "cs_distribution")
  expect_equal(calc_rci(rci_data_list, m_pre, sd_pre, m_post, sd_post, reliability)[["r_dd"]], r_dd)
  expect_equal(calc_rci(rci_data_list, m_pre, sd_pre, m_post, sd_post, reliability)[["se_measurement"]], se_measurement)
  expect_equal(calc_rci(rci_data_list, m_pre, sd_pre, m_post, sd_post, reliability)[["data"]][["rci"]], manual_rci)
})

