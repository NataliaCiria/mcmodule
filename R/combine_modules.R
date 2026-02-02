#' Combine Two Modules
#'
#' @description
#' Combines two mcmodules into a single mcmodule by merging their data and components.
#'
#' @param mcmodule_x First module to combine
#' @param mcmodule_y Second module to combine
#'
#' @return A combined mcmodule object
#'
#' @examples
#' module_x <- list(
#'   data = list(data_x = data.frame(x = 1:3)),
#'   node_list = list(
#'     node1 = list(type = "in_node"),
#'     node2 = list(type = "out_node")
#'   ),
#'   modules = c("module_x"),
#'   exp = quote({node2 <- node1 * 2})
#' )
#'
#' module_y <- list(
#'   data = list(data_y = data.frame(y = 4:6)),
#'   node_list = list(node3 = list(type = "out_node")),
#'   modules = c("module_y"),
#'   exp = quote({node3 <- node1 + node2})
#' )
#'
#' module_xy <- combine_modules(module_x, module_y)
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
    names(mcmodule$data) <- unique(names(c(mcmodule_x$data, mcmodule_y$data)))
  }

  # Combine model expressions
  mcmodule$exp <- list(
    mcmodule_x$exp,
    mcmodule_y$exp
  )
  names(mcmodule$exp) <- c(name_x, name_y)

  # Combine node lists and modules
  mcmodule$node_list <- c(mcmodule_x$node_list, mcmodule_y$node_list)

  # Set class and return
  class(mcmodule) <- "mcmodule"
  return(mcmodule)
}

#' Describe Monte Carlo Module Compostion
#'
#' @description
#' Determines whether an mcmodule is a raw (single) module or a composition of
#' multiple modules combined via `combine_modules()`. For combined modules,
#' returns the names of all component modules (recursively extracted up to one level).
#'
#' @param mcmodule An mcmodule object to check
#'
#' @return A list with elements:
#'   \itemize{
#'     \item `is_combined`: Logical. TRUE if module is combined, FALSE if raw
#'     \item `n_modules`: Integer. Number of component modules (1 for raw, >1 for combined)
#'     \item `module_names`: Character vector. Names of all component modules (recursive)
#'     \item `module_exp`: Data frame with columns `module` and `exp` showing all expressions per module
#'   }
#'
#' @details
#' A raw module has a single expression in `mcmodule$exp`.
#' A combined module has multiple expressions in `mcmodule$exp`, each representing
#' a component module that was combined via `combine_modules()`.
#'
#' For combined modules, module names are recursively extracted up to one level deep.
#' This allows identifying all base modules even in deeply nested combinations.
#'
#' @examples
#' # Raw module
#' raw_module <- eval_module(
#'   exp = c(imports = imports_exp),
#'   data = imports_data,
#'   mctable = imports_mctable,
#'   data_keys = imports_data_keys
#' )
#' mcmodule_composition(raw_module)
#'
#' # Combined module
#' combined_module <- combine_modules(raw_module, raw_module)
#' mcmodule_composition(combined_module)
#'
#' @export
mcmodule_composition <- function(mcmodule) {
  # Validate input
  if (!inherits(mcmodule, "mcmodule")) {
    stop("Input must be an mcmodule object")
  }

  if (!("exp" %in% names(mcmodule)) || is.null(mcmodule$exp)) {
    stop("mcmodule does not contain an 'exp' element")
  }

  # Function to recursively extract module names up to one level
  extract_module_names <- function(exp_list, level = 0) {
    if (level > 1) {
      return(character(0))
    }

    names_list <- names(exp_list)

    module_names <- lapply(seq_along(exp_list), \(i) {
      elem <- exp_list[[i]]
      elem_name <- names_list[i]

      if (is.list(elem) && !is.expression(elem) && !is.language(elem)) {
        child_is_list <- vapply(
          elem,
          \(x) is.list(x) && !is.expression(x) && !is.language(x),
          logical(1)
        )

        if (any(child_is_list) && level < 1L) {
          nested_names <- extract_module_names(elem, level = level + 1L)

          if (length(nested_names) > 0) {
            return(nested_names)
          }
        }

        return(elem_name)
      }

      elem_name
    })

    unlist(module_names)
  }

  # Function to extract all expressions with their module names
  extract_module_exp <- function(exp_list, parent_module = NULL, level = 0) {
    if (level > 1) {
      return(data.frame(
        module = character(0),
        exp = character(0),
        stringsAsFactors = FALSE
      ))
    }

    names_list <- names(exp_list)

    result_list <- lapply(seq_along(exp_list), \(i) {
      elem <- exp_list[[i]]
      elem_name <- names_list[i]

      if (is.list(elem) && !is.expression(elem) && !is.language(elem)) {
        # Check if children are lists (nested modules)
        child_is_list <- vapply(
          elem,
          \(x) is.list(x) && !is.expression(x) && !is.language(x),
          logical(1)
        )

        if (any(child_is_list) && level < 1L) {
          # Recurse into nested modules
          extract_module_exp(
            elem,
            parent_module = elem_name,
            level = level + 1L
          )
        } else {
          # This is a module with expressions
          exp_names <- names(elem)
          data.frame(
            module = rep(elem_name, length(elem)),
            exp = exp_names,
            stringsAsFactors = FALSE
          )
        }
      } else if (is.expression(elem) || is.language(elem)) {
        # This is an expression
        data.frame(
          module = if (!is.null(parent_module)) parent_module else elem_name,
          exp = elem_name,
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          module = character(0),
          exp = character(0),
          stringsAsFactors = FALSE
        )
      }
    })

    bind_rows(result_list)
  }

  # Combined modules have nested lists in exp
  has_nested <- any(vapply(
    mcmodule$exp,
    \(x) is.list(x) && !is.expression(x) && !is.language(x),
    logical(1)
  ))

  if (has_nested) {
    module_names <- extract_module_names(mcmodule$exp)
    n_modules <- length(module_names)
    is_combined <- n_modules > 1
    module_exp <- extract_module_exp(mcmodule$exp)
  } else {
    module_names <- deparse(substitute(mcmodule))
    n_modules <- 1L
    is_combined <- FALSE

    # For raw modules, extract expressions directly
    exp_names <- names(mcmodule$exp)
    module_exp <- data.frame(
      module = rep(module_names, length(exp_names)),
      exp = exp_names,
      stringsAsFactors = FALSE
    )
  }

  list(
    is_combined = is_combined,
    n_modules = n_modules,
    module_names = module_names,
    module_exp = module_exp
  )
}
