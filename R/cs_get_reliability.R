#' Get Reliability Of A clinisig Object
#'
#' @param x A clinisig object
#'
#' @importFrom dplyr tibble
#'
#' @return A tibble showing the reliability
#' @export
#'
#' @examples
#' results <- jacobson_1989 |>
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = gds,
#'     pre = "pre",
#'     reliability = 0.80,
#'     m_functional = 30,
#'     sd_functional = 10,
#'     type = "c"
#'   )
#'
#' results_nk <- jacobson_1989 |>
#'   clinical_significance(
#'     id = subject,
#'     time = time,
#'     outcome = gds,
#'     pre = "pre",
#'     reliability = 0.80,
#'     reliability_post = 0.85,
#'     m_functional = 30,
#'     sd_functional = 10,
#'     type = "c",
#'     method = "NK"
#'   )
#'
#' cs_get_reliability(results)
#' cs_get_reliability(results_nk)
cs_get_reliability <- function(x) {
  clinisig_method <- get_method(x)

  reliability <- tibble(
    reliability = x[["reliability"]]
  )

  if (clinisig_method == "NK") {
    reliability_post <- tibble(
      reliability_post = x[["rci_results"]][["reliability_post"]]
    )

    reliability <- reliability |>
      bind_cols(reliability_post) |>
      rename(reliability_pre = reliability)
  }

  reliability
}
