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
#' cs_get_data(results)
#' cs_get_data(results, "wide")
#' cs_get_data(results, "original")
cs_get_data <- function(x, dataset = "data") {
  if (!inherits(x, "clinisig")) cli::cli_abort("The supplied object must be of class {.code clinisig}.")

  x[["datasets"]][[dataset]]
}
