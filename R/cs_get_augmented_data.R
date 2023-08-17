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



#' Extract Augmented Data from a cs_statistical Object
#'
#' @param x An object of class `cs_distribution`
#' @param ... Additional arguments
#'
#' @return A tibble
#' @export
cs_get_augmented_data.cs_statistical <- function(x, ...) {
  cutoff_categories <- x[["cutoff_results"]][["data"]]
  used_data <- x[["datasets"]][["data"]]
  cs_method <- x[["method"]]


  # Join data with cutoff results
  joined_data <- used_data |>
    dplyr::left_join(cutoff_categories, dplyr::join_by("id"))


  # Build categories based on cs_method
  if (cs_method != "HA") {
    joined_data |>
      dplyr::mutate(
        category = dplyr::case_when(
          clinical_pre & functional_post ~ "Improved",
          !clinical_pre & !functional_post ~ "Deteriorated",
          !(clinical_pre & functional_post) & !(!clinical_pre & !functional_post) ~ "Unchanged"
        ),
        category = factor(category, levels = c("Improved", "Unchanged", "Deteriorated"))
      )
  } else {
    joined_data |>
      dplyr::mutate(
        category = dplyr::case_when(
          functional_post ~ "Improved",
          !functional_post ~ "Unchanged"
        ),
        category = factor(category, levels = c("Improved", "Unchanged"))
      )
  }
}



#' Extract Augmented Data from a cs_combined Object
#'
#' To obtain patient-wise results.
#'
#' This function returns the patient-wise results, containing the considered pre
#' and post intervention value, its raw change as well as the RCI and the
#' individual category a patient belongs to.
#'
#'
#' @return A tibble with used data and clinical significance categories
#' @export
#'
#' @examples
#' results <- jacobson_1989 |>
#' clinical_significance(
#'   id = subject,
#'   time = time,
#'   outcome = gds,
#'   pre = "pre",
#'   reliability = 0.80,
#'   m_functional = 30,
#'   sd_functional = 7,
#'   type = "c"
#' )
#'
#' cs_get_augmented_data(results)
cs_get_augmented_data.cs_combined <- function(x) {
  cs_method <- x[["method"]]
  categories <- x[["summary_table"]][["categories"]]


  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  categories |>
    dplyr::rename(rci_indiv = rci) |>
    dplyr::mutate(
      category = dplyr::case_when(
        recovered ~ "Recovered",
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated",
        harmed ~ "Harmed"
      ),
      category = factor(category, levels = c("Recovered", "Improved", "Unchanged", "Deteriorated", "Harmed"))
    )
}




#' Extract Augmented Data from a cs_percentage Object
#'
#' @param x An object of class `cs_percentage`
#' @param ... Additional arguments
#'
#' @return A tibble
#' @export
cs_get_augmented_data.cs_percentage <- function(x, ...) {
  pct_categories <- x[["pct_results"]]
  used_data <- x[["datasets"]][["data"]]

  used_data |>
    dplyr::left_join(pct_categories, dplyr::join_by("id")) |>
    dplyr::mutate(
      category = dplyr::case_when(
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated"
      ),
      category = factor(category, levels = c("Improved", "Unchanged", "Deteriorated"))
    )
}




#' Extract Augmented Data from a cs_anchor_individual Object
#'
#' @param x An object of class `cs_anchor_individual_within`
#' @param ... Additional arguments
#'
#' @return A tibble
#' @export
cs_get_augmented_data.cs_anchor_individual_within <- function(x, ...) {
  anchor_categories <- x[["anchor_results"]]
  used_data <- x[["datasets"]][["data"]]

  used_data |>
    dplyr::left_join(anchor_categories, dplyr::join_by("id")) |>
    dplyr::mutate(
      category = dplyr::case_when(
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated"
      ),
      category = factor(category, levels = c("Improved", "Unchanged", "Deteriorated"))
    )
}
