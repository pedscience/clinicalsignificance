clinisig_object <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85)

# Variable for calculation
s_diff <- clinisig_object$rci$s_diff

manual_results <- tibble(
  pre = c(0, 100),
  ymin = pre - 1.96 * s_diff,
  ymax = pre + 1.96 * s_diff
)

test_that("RCI data for JT method plotting is calculated correctly", {
  rci_data <- .generate_rci_data_jt(clinisig_object)

  expect_s3_class(rci_data, "tbl_df")
  expect_equal(rci_data, manual_results)
})
