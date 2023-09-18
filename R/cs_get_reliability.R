#' Get Reliability Of A cs_analysis Object
#'
#' @param x A cs_analysis object
#'
#' @family get
#' @return A tibble showing the reliability
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_distribution(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     reliability = 0.80
#'   )
#'
#'
#' cs_get_reliability(cs_results)
cs_get_reliability <- function(x) {
  .check_class(x)
  if (!inherits(x, "cs_distribution") & !inherits(x, "cs_combined")) cli::cli_abort("There was no reliability provided for this clinical significance method.")

  cs_method <- x[["method"]]

  reliability <- tibble::tibble(
    reliability = x[["reliability"]]
  )

  if (cs_method == "NK") {
    reliability_post <- tibble::tibble(
      reliability_post = x[["rci_results"]][["reliability_post"]]
    )

    reliability <- reliability |>
      dplyr::bind_cols(reliability_post) |>
      dplyr::rename(reliability_pre = reliability)
  }

  reliability
}
