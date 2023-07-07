#' Generic to Extract Augmented Data from a clinisig Object
#'
#' @param x A clinisig object
#' @param ... Additional arguments
#'
#' @return A tibble with augmented data
#' @export
cs_get_augmented_data <- function(x, ...) {
  UseMethod("cs_get_augmented_data")
}



#' Extract Augmented Data from a cs_distribution Object
#'
#' @param x An object of class `cs_distribution`
#' @param ... Additional arguments
#'
#' @return A tibble
#' @export
cs_get_augmented_data.cs_distribution <- function(x, ...) {
  if (!inherits(x, "clinisig")) cli::cli_abort("The supplied object must be of class {.code clinisig}.")

  rci_categories <- x[["rci_results"]][["data"]]
  used_data <- x[["datasets"]][["data"]]

  used_data |>
    dplyr::left_join(rci_categories, dplyr::join_by("id")) |>
    dplyr::mutate(
      category = dplyr::case_when(
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated"
      ),
      category = factor(category, levels = c("Improved", "Unchanged", "Deteriorated"))
    )
}



#' #' Get Data Augmented With Clinical Significance Categories
#' #'
#' #' To obtain patient-wise results, use `get_augmented_data()`.
#' #'
#' #' This function returns the patient-wise results, containing the considered pre
#' #' and post intervention value, its raw change as well as the RCI and the
#' #' individual category a patient belongs to.
#' #'
#' #' @inheritParams get_data
#' #'
#' #' @return A tibble with used data and clinical significance categories
#' #' @export
#' #'
#' #' @examples
#' #' results <- jacobson_1989 |>
#' #' clinical_significance(
#' #'   id = subject,
#' #'   time = time,
#' #'   outcome = gds,
#' #'   pre = "pre",
#' #'   reliability = 0.80,
#' #'   m_functional = 30,
#' #'   sd_functional = 7,
#' #'   type = "c"
#' #' )
#' #'
#' #' cs_get_augmented_data(results)
#' cs_get_augmented_data <- function(x) {
#'   assert_class(x, "clinisig")
#'
#'   cs_method <- cs_get_method(x)
#'
#'   if (cs_method == "HLM") {
#'     hlm_categories <- x[["categories"]]
#'     hlm_coefficients <- x[["rci"]][["coefficients"]] |>
#'       select(id, eb_estimate, sd)
#'
#'     categories <- hlm_categories |>
#'       dplyr::left_join(hlm_coefficients, by = "id") |>
#'       dplyr::relocate(eb_estimate:sd, .after = post)
#'   } else {
#'     categories <- x[["categories"]]
#'   }
#'
#'   categories |>
#'     dplyr::mutate(
#'       category = dplyr::case_when(
#'         recovered ~ "Recovered",
#'         improved ~ "Improved",
#'         unchanged ~ "Unchanged",
#'         deteriorated ~ "Deteriorated",
#'         harmed ~ "Harmed"
#'       ),
#'       category = factor(category, levels = c("Recovered", "Improved", "Unchanged", "Deteriorated", "Harmed"))
#'     )
#' }
