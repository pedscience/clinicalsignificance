# Example from Jacobson et al. (1991)
jacobson_m_pre <- 40
jacobson_sd_pre <- 7.5
jacobson_m_functional <- 60
jacobson_sd_functional <- 7.5

c <- (jacobson_sd_functional * jacobson_m_pre + jacobson_sd_pre * jacobson_m_functional) / (jacobson_sd_functional + jacobson_sd_pre)
a_higher <- jacobson_m_pre + 2 * jacobson_sd_pre
a_lower <- jacobson_m_pre - 2 * jacobson_sd_pre
b_higher <- jacobson_m_functional - 2 * jacobson_sd_pre
b_lower <- jacobson_m_functional + 2 * jacobson_sd_pre

test_that("cutoff is calculated correctly", {
  expect_equal(
    .calc_cutoff_jt(
      m_clinical = jacobson_m_pre,
      sd_clinical = jacobson_sd_pre,
      m_functional = jacobson_m_functional,
      sd_functional = jacobson_sd_functional,
      type = "c",
      direction = 1
    )[["value"]],
    c
  )

  expect_equal(
    .calc_cutoff_jt(
      m_clinical = jacobson_m_pre,
      sd_clinical = jacobson_sd_pre,
      m_functional = jacobson_m_functional,
      sd_functional = jacobson_sd_functional,
      type = "c",
      direction = -1
    )[["value"]],
    c
  )

  expect_equal(
    .calc_cutoff_jt(
      m_clinical = jacobson_m_pre,
      sd_clinical = jacobson_sd_pre,
      m_functional = jacobson_m_functional,
      sd_functional = jacobson_sd_functional,
      type = "a",
      direction = 1
    )[["value"]],
    a_higher
  )

  expect_equal(
    .calc_cutoff_jt(
      m_clinical = jacobson_m_pre,
      sd_clinical = jacobson_sd_pre,
      m_functional = jacobson_m_functional,
      sd_functional = jacobson_sd_functional,
      type = "a",
      direction = -1
    )[["value"]],
    a_lower
  )

  expect_equal(
    .calc_cutoff_jt(
      m_clinical = jacobson_m_pre,
      sd_clinical = jacobson_sd_pre,
      m_functional = jacobson_m_functional,
      sd_functional = jacobson_sd_functional,
      type = "b",
      direction = 1
    )[["value"]],
    b_higher
  )

  expect_equal(
    .calc_cutoff_jt(
      m_clinical = jacobson_m_pre,
      sd_clinical = jacobson_sd_pre,
      m_functional = jacobson_m_functional,
      sd_functional = jacobson_sd_functional,
      type = "b",
      direction = -1
    )[["value"]],
    b_lower
  )
})


# Example from Bauer et al. (2004)
bauer_m_pre <- 68.35
bauer_sd_pre <- 22.33
bauer_rel_pre <- 0.93
bauer_m_functional <- 50
bauer_sd_functional <- 22
bauer_rel_functional <- 0.93

c_true <- (bauer_sd_functional * sqrt(bauer_rel_functional) * bauer_m_pre + bauer_sd_pre * sqrt(bauer_rel_pre) * bauer_m_functional) / (bauer_sd_functional * sqrt(bauer_rel_functional) + bauer_sd_pre * sqrt(bauer_rel_pre))
a_true_higher <- bauer_m_pre + 2 * bauer_sd_pre * sqrt(bauer_rel_pre)
a_true_lower <- bauer_m_pre - 2 * bauer_sd_pre * sqrt(bauer_rel_pre)
b_true_higher <- bauer_m_functional - 2 * bauer_sd_pre * sqrt(bauer_rel_pre)
b_true_lower <- bauer_m_functional + 2 * bauer_sd_pre * sqrt(bauer_rel_pre)


test_that("HA cutoff is calculated correctly", {
  expect_equal(
    .calc_cutoff_ha(
      m_clinical = bauer_m_pre,
      sd_clinical = bauer_sd_pre,
      reliability_clinical = bauer_rel_pre,
      m_functional = bauer_m_functional,
      sd_functional = bauer_sd_functional,
      reliability_functional = bauer_rel_functional,
      type = "c",
      direction = 1
    )[["value"]],
    c_true
  )

  expect_equal(
    .calc_cutoff_ha(
      m_clinical = bauer_m_pre,
      sd_clinical = bauer_sd_pre,
      reliability_clinical = bauer_rel_pre,
      m_functional = bauer_m_functional,
      sd_functional = bauer_sd_functional,
      reliability_functional = bauer_rel_functional,
      type = "c",
      direction = -1
    )[["value"]],
    c_true
  )

  expect_equal(
    .calc_cutoff_ha(
      m_clinical = bauer_m_pre,
      sd_clinical = bauer_sd_pre,
      reliability_clinical = bauer_rel_pre,
      m_functional = bauer_m_functional,
      sd_functional = bauer_sd_functional,
      reliability_functional = bauer_rel_functional,
      type = "a",
      direction = 1
    )[["value"]],
    a_true_higher
  )

  expect_equal(
    .calc_cutoff_ha(
      m_clinical = bauer_m_pre,
      sd_clinical = bauer_sd_pre,
      reliability_clinical = bauer_rel_pre,
      m_functional = bauer_m_functional,
      sd_functional = bauer_sd_functional,
      reliability_functional = bauer_rel_functional,
      type = "a",
      direction = -1
    )[["value"]],
    a_true_lower
  )

  expect_equal(
    .calc_cutoff_ha(
      m_clinical = bauer_m_pre,
      sd_clinical = bauer_sd_pre,
      reliability_clinical = bauer_rel_pre,
      m_functional = bauer_m_functional,
      sd_functional = bauer_sd_functional,
      reliability_functional = bauer_rel_functional,
      type = "b",
      direction = 1
    )[["value"]],
    b_true_higher
  )

  expect_equal(
    .calc_cutoff_ha(
      m_clinical = bauer_m_pre,
      sd_clinical = bauer_sd_pre,
      reliability_clinical = bauer_rel_pre,
      m_functional = bauer_m_functional,
      sd_functional = bauer_sd_functional,
      reliability_functional = bauer_rel_functional,
      type = "b",
      direction = -1
    )[["value"]],
    b_true_lower
  )
})
