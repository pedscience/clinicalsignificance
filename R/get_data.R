#' Get Data From A clinisig Object
#'
#' @param x A clinisig object.
#' @param dataset The dataset you wish to retrieve. Available options are
#'   `"original"` (the raw original dataset), `"wide"` (the original dataset
#'   in wide format), and `"data"` (the dataset which is used in the
#'   calculations). The default is `"data"`
#'
#' @return A tibble
#' @export
get_data <- function(x, dataset = "data") {
  assert_class(x, "clinisig")

  x[["datasets"]][[dataset]]
}
