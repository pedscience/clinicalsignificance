get_clinical_significance_method <- function(x) {
  if (length(x[["method"]]) > 1) {
    x[["method"]][[1]]
  } else {
    x[["method"]]
  }
}
