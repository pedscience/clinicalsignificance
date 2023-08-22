#' Get Data From A cs_analysis Object
#'
#' @param x A cs_analysis object.
#' @param dataset The dataset you wish to retrieve. Available options are
#'   - `"original"` (the raw original dataset)
#'   - `"wide"` (the original dataset in wide format)
#'   - `"data"` (the dataset which is used in the calculations).
#'   The default is `"data"`
#'
#' @family get
#'
#' @return A tibble
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_anchor(id, time, bdi, mid_improvement = 9, pre = 1, post = 4)
#'
#' cs_get_data(cs_results)
#' cs_get_data(cs_results, dataset = "wide")
#' cs_get_data(cs_results, dataset = "original")
cs_get_data <- function(x, dataset = "data") {
  .check_class(x)

  x[["datasets"]][[dataset]]
}
