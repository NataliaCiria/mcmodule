#' Set or Get Global Data Keys
#'
#' Manages a global data model by either setting new data keys or retrieving the current ones.
#' The data model consists of named lists containing column names and their associated key columns.
#'
#' @param data_keys Optional list of lists. Each inner list must contain:
#'   \itemize{
#'     \item cols: A vector containing the column names for the data
#'     \item keys: A vector specifying the key columns
#'   }
#'   If NULL, returns the current data model.
#'
#' @return
#'   \itemize{
#'     \item If data_keys = NULL: Returns the current global data model
#'     \item If data_keys provided: Sets the new data model and returns invisibly
#'   }
#' @examples
#' print(imports_data_keys)
#' set_data_keys(imports_data_keys)
#' @export
set_data_keys <- function(data_keys = NULL) {
  if (is.null(data_keys)) {
    if (!exists("data_keys", envir = .pkgglobalenv)) {
      empty_model <- list()
      assign("data_keys", empty_model, envir = .pkgglobalenv)
    }
    return(get("data_keys", envir = .pkgglobalenv))
  } else {
    # Validate data model structure
    if (!is.list(data_keys)) {
      stop("Data model must be a list")
    }

    # Check each element has required structure
    for (name in names(data_keys)) {
      element <- data_keys[[name]]
      if (is.null(element)) {
        next  # Skip NULL elements (conditionally included datasets)
      }
      if (!is.list(element) ||
          !all(c("cols", "keys") %in% names(element)) ||
          !is.vector(element$cols) ||
          !is.vector(element$keys)) {
        stop("Each data model element must be a list with 'cols' (column names vector) and 'keys' (vector of key columns)")
      }

      # Validate that all keys exist in cols
      if (!all(element$keys %in% element$cols)) {
        missing_keys <- element$keys[!element$keys %in% element$cols]
        stop("Keys must be a subset of column names. Missing in cols: ",
             paste(missing_keys, collapse = ", "),
             " for dataset: ", name)
      }
    }

    # Assign validated data model
    assign("data_keys", data_keys, envir = .pkgglobalenv)
    message("data_keys set to ", deparse(substitute(data_keys)))
  }
}

#' Reset Data Keys
#'
#' @description
#' Resets the data model to an empty list
#'
#' @return No return value, resets data keys
#' @examples
#' reset_data_keys()
#' @export
reset_data_keys <- function() {
  empty_model <- list()
  assign("data_keys", empty_model, envir = .pkgglobalenv)
  message("data_keys reset")
}
