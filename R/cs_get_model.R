#' Get The HLM Model From A cs_analysis Object
#'
#' @description With `cs_get_model()` you can extract the fitted HLM model for
#'   the distribution-based approach. This is useful to either diagnose the
#'   model further (beacuse all assumptions of HLMs apply in this case) or plot
#'   the results differently.
#'
#' @param x A cs_analysis object
#'
#' @family get
#'
#' @return A model of class `lmerMod`
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_distribution(id, time, bdi, rci_method = "HLM")
#'
#' cs_get_model(cs_results)
cs_get_model <- function(x) {
  .check_class(x)
  if (inherits(x, "cs_distribution") & !inherits(x, "cs_hlm")) cli::cli_abort("The cs_analysis object does not contain an HLM model.")

  x[["rci_results"]][["model"]]
}
