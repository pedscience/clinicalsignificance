#' Get Reliability Of A clinisig Object
#'
#' @param x A clinisig object
#'
#' @importFrom dplyr tibble
#'
#' @return A tibble conatining the reliability
#' @export
get_reliability <- function(x) {
  tibble(
    reliability = x[["reliability"]]
  )
}
