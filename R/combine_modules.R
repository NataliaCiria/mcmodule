#' Combine Two Modules
#'
#' This function combines two mcmodules into a single mcmodule, merging their data,
#' model expressions, node lists, and other components.
#'
#' @param mcmodule_x First module to combine
#' @param mcmodule_y Second module to combine
#'
#' @return A combined mcmodule object containing the elements elements from both input modules
#'
#' @export
combine_modules <- function(mcmodule_x, mcmodule_y) {
  mcmodule <- list()

  # Extract names of input modules
  name_x <- deparse(substitute(mcmodule_x))
  name_y <- deparse(substitute(mcmodule_y))

  # Combine data based on structure
  if (identical(mcmodule_x$data, mcmodule_y$data)) {
    mcmodule$data <- mcmodule_x$data
  } else {
    mcmodule$data <- unique(c(mcmodule_x$data, mcmodule_y$data))
  }

  # Combine model expressions
  mcmodule$model_expression <- list(
    mcmodule_x$model_expression,
    mcmodule_y$model_expression
  )
  names(mcmodule$model_expression) <- c(name_x, name_y)

  # Combine node lists and modules
  mcmodule$node_list <- c(mcmodule_x$node_list, mcmodule_y$node_list)
  mcmodule$modules <- unique(c(mcmodule_x$modules, mcmodule_y$modules))

  # Combine mc_list if they exist
  if (!is.null(mcmodule_x$mc_list)) {
    mcmodule$mc_list <- c(
      mcmodule_x$mc_list,
      mcmodule_y$mc_list
    )
  }

  # Set class and return
  class(mcmodule) <- "mcmodule"
  return(mcmodule)
}
