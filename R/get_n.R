#' Get Number Of Participants From A clinisig Object
#'
#' @param x A clinisig object
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
#' get_n(results)
#' get_n(results, which = "original")
#' get_n(results, which = "used")
get_n <- function(x, which = "all") {
  assert_class(x, "clinisig")

  if (which == "all") {
    x[["n_obs"]] |>
      as_tibble() |>
      mutate(
        percent_used = n_used / n_original
      )
  } else if (which == "original") {
    tibble(
      n_original = x[["n_obs"]][["n_original"]]
    )
  } else if (which == "used") {
    tibble(
      n_used = x[["n_obs"]][["n_used"]]
    )
  }
}
