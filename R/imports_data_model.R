#' Sample Data Model
#'
#' A hierarchical data structure containing test sensitivity, animal import, and regional
#' prevalence information, and its keys
#'
#' @format A list with three components:
#' \describe{
#'   \item{test_sensitivity}{List containing test sensitivity data and "pathogen" as key}
#'   \item{animal_imports}{List containing animal import data and "origin" as key}
#'   \item{prevalence_region}{List containing prevalence data with "pathogen" and "origin" as keys}
#' }
"imports_data_keys"
