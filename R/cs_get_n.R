#' Get Number Of Participants From A cs_analysis Object
#'
#' @param x A cs_analysis object
#' @param which Which n should be returned? Available options are
#' - `"all"`, n in the original and used data set (the default)
#' - `"original"`, n in the original dataset
#' - `"used"`, n in the used data set, so after conversion to wide format and
#' omitting cases with missing values
#'
#'
#' @importFrom dplyr tibble as_tibble
#' @importFrom checkmate assert_class
#'
#' @return A tibble with number of participants
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_anchor(id, time, bdi, mid_improvement = 9, pre = 1, post = 4)
#'
#' cs_get_n(cs_results)
#' cs_get_n(cs_results, "original")
#' cs_get_n(cs_results, "used")
cs_get_n <- function(x, which = "all") {
  .check_class(x)


  if (which == "all") {
    x[["n_obs"]] |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        percent_used = n_used / n_original
      )
  } else if (which == "original") {
    dplyr::tibble(
      n_original = x[["n_obs"]][["n_original"]]
    )
  } else if (which == "used") {
    dplyr::tibble(
      n_used = x[["n_obs"]][["n_used"]]
    )
  }
}
