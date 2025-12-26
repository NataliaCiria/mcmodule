#' Find mcnodes in a mcmodule based on a condition
#'
#' Generic function that applies a test function to each mcnode in an mcmodule
#' and returns the names of mcnodes where the test returns TRUE. Useful for
#' debugging and troubleshooting Monte Carlo models.
#'
#' @param mcmodule An mcmodule object containing node_list with mcnodes
#' @param test_func A function that takes an mcnode and returns a logical value
#'   (TRUE if the condition is met)
#'
#' @return A character vector containing the names of mcnodes where test_func
#'   returns TRUE. Returns an empty character vector if no mcnodes meet the condition.
#'
#' @examples
#' # Find nodes with negative values
#' which_mcnode(imports_mcmodule, function(x) any(x < 0, na.rm = TRUE))
#'
#' # Find nodes with values greater than 1
#' which_mcnode(imports_mcmodule, function(x) any(x > 1, na.rm = TRUE))
#'
#' @export
#'
#' @seealso \code{\link{which_mcnode_na}}, \code{\link{which_mcnode_inf}}
which_mcnode <- function(mcmodule, test_func) {
  # Validate input
  if (!is.list(mcmodule) || is.null(mcmodule$node_list)) {
    stop("mcmodule must be a list with a node_list component")
  }
  
  if (!is.function(test_func)) {
    stop("test_func must be a function")
  }

  node_names <- names(mcmodule$node_list)

  # Handle empty node_list
  if (length(node_names) == 0) {
    return(character(0))
  }

  # Apply test function to each node
  test_results <- sapply(mcmodule$node_list, function(node) {
    if (is.null(node[["mcnode"]])) {
      return(FALSE)
    }
    tryCatch({
      test_func(node[["mcnode"]])
    }, error = function(e) {
      FALSE
    })
  })

  # Return names of nodes where test is TRUE
  return(names(test_results[test_results == TRUE]))
}


#' Find mcnodes in a mcmodule that contain NAs
#'
#' Identifies which mcnodes within an mcmodule contain NA values.
#' Useful for troubleshooting and debugging Monte Carlo models to find
#' nodes that may be causing issues due to missing or undefined values.
#'
#' @param mcmodule An mcmodule object containing node_list with mcnodes
#'
#' @return A character vector containing the names of mcnodes that have NA values.
#'   Returns an empty character vector if no NAs are found.
#'
#' @examples
#' # Find nodes with NAs in the imports_mcmodule
#' which_mcnode_na(imports_mcmodule)
#'
#' # Create a test mcmodule with NAs
#' test_mcnode_na <- mcstoc(runif,
#'                min = mcdata(c(0.1, NA, 0.3), type = "0", nvariates = 3),
#'                max = mcdata(c(0.2, 0.4, 0.5), type = "0", nvariates = 3),
#'                nvariates = 3
#' )
#' test_mcnode_clean <- mcstoc(runif,
#'                min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
#'                max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
#'                nvariates = 3
#' )
#' test_mcmodule <- list(
#'   node_list = list(
#'     node_a = list(mcnode = test_mcnode_na),
#'     node_b = list(mcnode = test_mcnode_clean)
#'   )
#' )
#' which_mcnode_na(test_mcmodule)
#'
#' @export
#'
#' @seealso \code{\link{which_mcnode}}, \code{\link{which_mcnode_inf}}, \code{\link{mcnode_na_rm}}
which_mcnode_na <- function(mcmodule) {
  which_mcnode(mcmodule, function(x) any(is.na(x)))
}


#' Find mcnodes in a mcmodule that contain infinite values
#'
#' Identifies which mcnodes within an mcmodule contain infinite values (Inf or -Inf).
#' Useful for troubleshooting and debugging Monte Carlo models to find
#' nodes that may be causing issues due to infinite values.
#'
#' @param mcmodule An mcmodule object containing node_list with mcnodes
#'
#' @return A character vector containing the names of mcnodes that have infinite values.
#'   Returns an empty character vector if no infinite values are found.
#'
#' @examples
#' # Find nodes with infinite values in the imports_mcmodule
#' which_mcnode_inf(imports_mcmodule)
#'
#' # Create a test mcmodule with Inf values
#' test_mcnode_inf <- mcstoc(runif,
#'                min = mcdata(c(0.1, -Inf, 0.3), type = "0", nvariates = 3),
#'                max = mcdata(c(Inf, 0.4, 0.5), type = "0", nvariates = 3),
#'                nvariates = 3
#' )
#' test_mcnode_clean <- mcstoc(runif,
#'                min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
#'                max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
#'                nvariates = 3
#' )
#' test_mcmodule <- list(
#'   node_list = list(
#'     node_a = list(mcnode = test_mcnode_inf),
#'     node_b = list(mcnode = test_mcnode_clean)
#'   )
#' )
#' which_mcnode_inf(test_mcmodule)
#'
#' @export
#'
#' @seealso \code{\link{which_mcnode}}, \code{\link{which_mcnode_na}}, \code{\link{mcnode_na_rm}}
which_mcnode_inf <- function(mcmodule) {
  which_mcnode(mcmodule, function(x) any(is.infinite(x)))
}
