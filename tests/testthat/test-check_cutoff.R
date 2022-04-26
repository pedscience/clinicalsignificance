test_that("cutoff plot is correct", {
  object_cutoff <- check_cutoff(clinical_significance(claus_2020, id, time, bdi, pre = 1, post = 4, reliability = 0.80, m_functional = 8, sd_functional = 8, type = "c"))
  manual_cutoff_a <- check_cutoff(m_clinical = 60, sd_clinical = 8, m_functional = 10, sd_functional = 8, type = "a")
  manual_cutoff_b <- check_cutoff(m_clinical = 60, sd_clinical = 8, m_functional = 10, sd_functional = 8, type = "b")
  manual_cutoff_c <- check_cutoff(m_clinical = 60, sd_clinical = 8, m_functional = 10, sd_functional = 8, type = "c")

  expect_doppelganger("cutoff from clinisig object", object_cutoff)
  expect_doppelganger("manual cutoff a", manual_cutoff_a)
  expect_doppelganger("manual cutoff b", manual_cutoff_b)
  expect_doppelganger("manual cutoff c", manual_cutoff_c)
})


test_that("check cutoff throws expected errors", {
  expect_snapshot_error(check_cutoff(clinical_significance(claus_2020, id, time, bdi, pre = 1, post = 4, reliability = 0.80, method = "HA")))
})
