#' Find mcnodes in a mcmodule that contain NAs
#'
#' This function identifies which mcnodes within an mcmodule contain NA values.
#' This is useful for troubleshooting and debugging Monte Carlo models to find
#' nodes that may be causing issues due to missing or undefined values.
#'
#' @param mcmodule An mcmodule object containing node_list with mcnodes
#'
#' @return A character vector containing the names of mcnodes that have NA values.
#'   Returns an empty character vector if no NAs are found.
#'
#' @examples
#' \dontrun{
#' # Find nodes with NAs in the imports_mcmodule
#' mcmodule_nas(imports_mcmodule)
#' }
#'
#' @export
#'
#' @seealso \code{\link{mcnode_na_rm}} for replacing NA values in mcnodes
mcmodule_nas <- function(mcmodule) {
  # Validate input
  if (!is.list(mcmodule) || is.null(mcmodule$node_list)) {
    stop("mcmodule must be a list with a node_list component")
  }

  node_names <- names(mcmodule$node_list)

  # Extract mcnode objects from each node
  mcnodes_na <- sapply(mcmodule$node_list[node_names], "[[", "mcnode")

  # Check for NA values in each mcnode
  mcnodes_na <- sapply(mcnodes_na, "is.na")

  # Check if any NA values exist in each mcnode
  mcnodes_na <- sapply(mcnodes_na, "any")

  # Return names of nodes containing NAs
  return(names(mcnodes_na[mcnodes_na]))
}
