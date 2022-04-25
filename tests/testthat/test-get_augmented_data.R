clinisig_object_jt <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, method = "JT")
clinisig_object_gln <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, method = "GLN")
clinisig_object_hll <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, method = "HLL")
clinisig_object_en <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, method = "EN")
clinisig_object_nk <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, reliability_post = 0.90, method = "NK")
clinisig_object_ha <- clinical_significance(jacobson_1989, subject, time, gds, pre = "pre", reliability = 0.85, method = "HA")
clinisig_object_hlm <- clinical_significance(anxiety, subject, measurement, anxiety, method = "HLM")

names_jt <- c("id", "pre", "post", "change", "rci", "clinical_pre", "functional_post", "recovered", "improved", "unchanged", "deteriorated", "harmed", "category")
names_en <- c("id", "pre", "post", "change", "pre_true", "lower", "upper", "clinical_pre", "functional_post", "recovered", "improved", "unchanged", "deteriorated", "harmed", "category")
names_ha <- c("id", "pre", "post", "change", "cs_indiv", "rci", "functional_post", "recovered", "improved", "unchanged", "deteriorated", "harmed", "category")
names_hlm <- c("id", "n", "pre", "post", "intercept", "slope", "eb_slope", "rci", "clinical_pre", "functional_post", "recovered", "improved", "unchanged", "deteriorated", "harmed", "category")

test_that("data is augmented correctly", {
  # JT
  augmented_jt <- get_augmented_data(clinisig_object_jt)
  expect_s3_class(augmented_jt, "tbl_df")
  expect_equal(names(augmented_jt), names_jt)

  # GLN
  augmented_gln <- get_augmented_data(clinisig_object_gln)
  expect_s3_class(augmented_gln, "tbl_df")
  expect_equal(names(augmented_gln), names_jt)

  # HLL
  augmented_hll <- get_augmented_data(clinisig_object_hll)
  expect_s3_class(augmented_hll, "tbl_df")
  expect_equal(names(augmented_hll), names_jt)

  # EN
  augmented_en <- get_augmented_data(clinisig_object_en)
  expect_s3_class(augmented_en, "tbl_df")
  expect_equal(names(augmented_en), names_en)

  # NK
  augmented_nk <- get_augmented_data(clinisig_object_nk)
  expect_s3_class(augmented_nk, "tbl_df")
  expect_equal(names(augmented_nk), names_jt)

  # HA
  augmented_ha <- get_augmented_data(clinisig_object_ha)
  expect_s3_class(augmented_ha, "tbl_df")
  expect_equal(names(augmented_ha), names_ha)

  # HLM
  augmented_hlm <- get_augmented_data(clinisig_object_hlm)
  expect_s3_class(augmented_hlm, "tbl_df")
  expect_equal(names(augmented_hlm), names_hlm)
})
