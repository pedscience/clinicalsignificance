#' Get Number Of Participants From A clinisig Object
#'
#' @param x A clinisg object
#' @param which Which n should be returned? Available options are `"all"` (n
#'   in the original and used data set), `"original"` (n in the original
#'   dataset), and `"used"` (n in the used data set, so after conversion to
#'   wide format and omitting cases with missing values). The default is `"all"`
#'
#' @importFrom dplyr tibble as_tibble
#' @importFrom rlang .data
#'
#' @return A tibble with number of participants
#' @export
get_n <- function(x, which = "all") {
  assert_class(x, "clinisig")

  if (which == "all") {
    x[["n_obs"]] %>%
      as_tibble() %>%
      mutate(
        percent_used = .data$n_used / .data$n_original
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
