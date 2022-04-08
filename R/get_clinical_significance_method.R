get_clinical_significance_method <- function(x) {
  assert_class(x, "clinisig")

  if (length(x[["method"]]) > 1) {
    x[["method"]][[1]]
  } else {
    x[["method"]]
  }
}
