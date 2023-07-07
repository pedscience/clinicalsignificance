clinisig_object <- clinical_significance(claus_2020, id, time, bdi, pre = 1, post = 4, reliability = 0.80)


test_that("get_n retrieves correct numbers", {
  expect_equal(get_n(clinisig_object)[["n_original"]], 43)
  expect_equal(get_n(clinisig_object)[["n_used"]], 40)
  expect_equal(get_n(clinisig_object)[["percent_used"]], 40/43)

  expect_equal(get_n(clinisig_object, "original")[[1]], 43)
  expect_equal(get_n(clinisig_object, "used")[[1]], 40)
})
