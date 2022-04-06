#' Get Reliability Of A clinisig Object
#'
#' @param x A clinisig object
#'
#' @importFrom dplyr tibble
#'
#' @return A tibble conatining the reliability
#' @export
get_reliability <- function(x) {
  clinisig_method <- get_clinical_significance_method(x)

  reliability <- tibble(
    reliability = x[["reliability"]]
  )

  if (clinisig_method == "NK") {
    reliability_post <- tibble(
      reliability_post = x[["rci"]][["reliability_post"]]
    )

    reliability <- reliability %>%
      bind_cols(reliability_post) %>%
      rename(reliability_pre = reliability)
  }

  reliability
}
