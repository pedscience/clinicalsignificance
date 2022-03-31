test_that("cutoff is calculated correctly", {
  expect_type(.calc_cutoff(m_clinical = 10, sd_clinical = 5, m_functional = NA, sd_functional = NA, type = "a"), "list")
  expect_equal(.calc_cutoff(m_clinical = 10, sd_clinical = 5, m_functional = NA, sd_functional = NA, type = "a")[["value"]], 20)
  expect_equal(.calc_cutoff(m_clinical = NA, sd_clinical = 5, m_functional = 20, sd_functional = 5, type = "b")[["value"]], 10)
})
