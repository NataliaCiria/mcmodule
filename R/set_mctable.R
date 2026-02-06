.pkgglobalenv <- new.env(parent = emptyenv())

#' Set or Get Monte Carlo Inputs Table
#'
#' @description
#' Manages a Monte Carlo inputs table in the global package environment by either setting new data
#' or retrieving the current table. The table stores information about Monte Carlo nodes including
#' their descriptions, functions, and dependencies.
#'
#' @param data Optional data frame containing MC table information. Must contain an 'mcnode' column.
#'            Other columns will be auto-filled if missing. If NULL, returns the current MC table.
#'
#' @return
#'   \itemize{
#'     \item If data = NULL: Returns the current MC table
#'     \item If data provided: Sets the new MC table and returns invisibly
#'   }
#'   The table contains the following columns:
#'   \itemize{
#'     \item mcnode - Character. Name of the Monte Carlo node (required)
#'     \item description - Character. Description of the parameter
#'     \item mc_func - Character. Probability distribution
#'     \item from_variable - Character. Variable name in the data table, if it is in a column with a name different from the mcnode
#'     \item transformation - Character. Transformation to be applied to the original column values
#'     \item sensi_baseline - Character. Parameters for baseline mock distribution
#'     \item sensi_variation - Character. OAT variation expression using 'value' placeholder
#'   }
#'
#' @examples
#' # Get current MC table
#' current_table <- set_mctable()
#'
#' # Set new MC table
#' mct <- data.frame(
#'   mcnode = c("h_prev", "w_prev"),
#'   description = c("Herd prevalence", "Within herd prevalence"),
#'   mc_func = c("runif", "runif"),
#'   sensi_baseline = c("min = 0, max = 1", "min = 0, max = 1"),
#'   sensi_variation = c("pmin(1, pmax(0, value * 1.5))", "pmin(1, pmax(0, value * 1.5))")
#' )
#' set_mctable(mct)
#'
#' @export
set_mctable <- function(data = NULL) {
  # Check if mctable exists, if not create with default values
  if (!exists("mctable", envir = .pkgglobalenv)) {
    assign(
      "mctable",
      data.frame(
        mcnode = character(),
        description = character(),
        mc_func = character(),
        from_variable = character(),
        transformation = character(),
        sensi_baseline = character(),
        sensi_variation = character()
      ),
      envir = .pkgglobalenv
    )
  }

  # Get current mctable
  mct <- get("mctable", envir = .pkgglobalenv)

  # If data provided, perform checks and auto-fill
  if (!is.null(data)) {
    data_name <- deparse(substitute(data))
    data <- check_mctable(data)
    assign("mctable", data, envir = .pkgglobalenv)
    message("mctable set to ", data_name)
  } else {
    # Return current mctable
    return(get("mctable", envir = .pkgglobalenv))
  }
}

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
    sensi_baseline = character(),
    sensi_variation = character()
  )
  assign("mctable", empty_mctable, envir = .pkgglobalenv)
  return(get("mctable", envir = .pkgglobalenv))
}


#' Checks mctable data
#'
#' @param data A data frame containing MC table information. Must contain an 'mcnode' column.
#'
#' @return A data frame with the standard mctable structure

check_mctable <- function(data) {
  # If data provided, perform checks and auto-fill
  if (is.data.frame(data)) {
    # Check if mcnode column exists
    if (!"mcnode" %in% colnames(data)) {
      stop("mcnode column not found in the mctable")
    }
    # Warn if mc_func column is missing
    if (!"mc_func" %in% colnames(data)) {
      warning(
        "No mc_func column found in the mctable. All nodes will be treated as deterministic (no uncertainty)."
      )
    }

    # Check columns and auto-fill with NA if missing
    cols <- c(
      "mc_func",
      "description",
      "from_variable",
      "transformation",
      "sensi_baseline",
      "sensi_variation"
    )

    missing_cols <- cols[!cols %in% colnames(data)]

    data[missing_cols] <- NA

    return(data)
  } else {
    stop("Data must be a data frame")
  }
}
