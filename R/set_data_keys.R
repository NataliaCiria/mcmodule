#' Set Data Keys
#'
#' @description
#' Sets or returns the global data model
#'
#' @param data_keys Optional list of lists containing data frames and their keys
#' @return Current data model
#'
#' @export
set_data_keys <- function(data_keys = NULL) {
  if (is.null(data_keys)) {
    if (!exists("data_keys", envir = .pkgglobalenv)) {
      empty_model <- list()
      assign("data_keys", empty_model, envir = .pkgglobalenv)
    }
  } else {
    # Validate data model structure
    if (!is.list(data_keys)) {
      stop("Data model must be a list")
    }

    # Check each element has required structure
    for (name in names(data_keys)) {
      element <- data_keys[[name]]
      if (!is.list(element) ||
        !all(c("data", "keys") %in% names(element)) ||
        !is.data.frame(element$data) ||
        !is.vector(element$keys)) {
        stop("Each data model element must be a list with 'data' (data frame) and 'keys' (vector)")
      }
    }

    # Assign validated data model
    assign("data_keys", data_keys, envir = .pkgglobalenv)
  }

  return(get("data_keys", envir = .pkgglobalenv))
}

#' Reset Data Model
#'
#' @description
#' Resets the data model to an empty list
#'
#' @return An empty list
#'
#' @export
reset_data_keys <- function() {
  empty_model <- list()
  assign("data_keys", empty_model, envir = .pkgglobalenv)
  return(get("data_keys", envir = .pkgglobalenv))
}
