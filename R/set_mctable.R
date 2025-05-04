#' Create or Modify an Monte Carlo Inputs Table
#'
#' @description
#' Creates a new input mcnodes table or modifies an existing one in the global package environment.
#' The table stores information about Monte Carlo nodes including their descriptions,
#' functions, dependencies, and sensitivity analysis settings.
#'
#' @param data A data frame containing MC table information. If NULL, returns the current MC table.
#'             Must contain an 'mcnode' column. Other columns will be auto-filled if missing.
#'
#' @return A data frame containing the MC table with the following columns:
#'   \itemize{
#'     \item mcnode - Character. Name of the Monte Carlo node (required)
#'     \item description - Character. Description of the node
#'     \item mc_func - Character. Monte Carlo function to be used
#'     \item from_variable - Character. Dependency on other variables
#'     \item transformation - Character. Transformation to be applied
#'     \item sensi_analysis - Logical. Whether to include in sensitivity analysis
#'   }
#'
#' @examples
#' mct <- data.frame(
#'   mcnode = c("h_prev", "w_prev"),
#'   description = c("Herd prevalence", "Within herd prevalence"),
#'   mc_func = c("runif", "runif"),
#'   sensi_analysis = c(TRUE, TRUE)
#' )
#' mctable(mct)
#'
#' @export

.pkgglobalenv <- new.env(parent=emptyenv())

set_mctable <- function(data = NULL) {
  # Check if mctable exists, if not create with default values
  if(!exists("mctable", envir=.pkgglobalenv)) {
    assign("mctable", data.frame(
      mcnode = character(),
      description = character(),
      mc_func = character(),
      from_variable = character(),
      transformation = character(),
      sensi_analysis = logical()
    ), envir=.pkgglobalenv)
    }

  # Get current mctable
  mct <- get("mctable", envir=.pkgglobalenv)

  # If data provided, perform checks and auto-fill
  if(!is.null(data)) {
    if(is.data.frame(data)) {
      # Check if mcnode column exists
      if(!"mcnode" %in% colnames(data)) {
        stop("mcnode column not found in the mctable")
      }

      # Check required columns and auto-fill if missing
      required_cols <- c("description", "mc_func", "from_variable", "transformation", "sensi_analysis")
      missing_cols <- required_cols[!required_cols %in% colnames(data)]

      if(length(missing_cols) > 0) {
        warning("The following columns were not specified and will be filled with default values: ",
                paste(missing_cols, collapse=", "))

        # Add missing columns with default values
        for(col in missing_cols) {
          if(col == "sensi_analysis") {
            data[[col]] <- FALSE
          } else {
            data[[col]] <- NA
          }
        }
      }

      assign("mctable", data, envir=.pkgglobalenv)
    } else {
      stop("Data must be a data frame")
    }
  }

  # Return current mctable
  return(get("mctable", envir=.pkgglobalenv))
}

#' Reset Monte Carlo Inputs Table
#'
#' @description
#' Resets the Monte Carlo inputs table
#'
#' @return An empty data frame with the standard mctable structure
#'
#' @export

reset_mctable <- function() {
  empty_mctable <- data.frame(
    mcnode = character(),
    description = character(),
    mc_func = character(),
    from_variable = character(),
    transformation = character(),
    sensi_analysis = logical()
  )
  assign("mctable", empty_mctable, envir=.pkgglobalenv)
  return(get("mctable", envir=.pkgglobalenv))
}
