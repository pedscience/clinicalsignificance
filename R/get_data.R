#' Get Data From A clinisig Object
#'
#' @param x A clinisig object.
#' @param dataset The dataset you wish to retrieve. Available options are
#'   - `"original"` (the raw original dataset)
#'   - `"wide"` (the original dataset in wide format)
#'   - `"data"` (the dataset which is used in the calculations).
#'   The default is `"data"`
#'
#' @return A tibble
#' @export
#'
#' @examples
#' results <- jacobson_1989 %>%
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
#' get_data(results)
#' get_data(results, "wide")
#' get_data(results, "original")
get_data <- function(x, dataset = "data") {
  assert_class(x, "clinisig")

  x[["datasets"]][[dataset]]
}
