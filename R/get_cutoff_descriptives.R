#' Get Descriptives Used In The Cutoff Calculation
#'
#' @param x A clinisg object
#'
#' @importFrom rlang .data
#'
#' @return A tibble with means and standard deviations of the clinical and
#'   functional population
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
#' get_cutoff_descriptives(results)

get_cutoff_descriptives <- function(x) {
  assert_class(x, "clinisig")

  x[["cutoff"]][["info"]] %>%
    as_tibble() %>%
    select(-.data$type, -.data$value)
}
