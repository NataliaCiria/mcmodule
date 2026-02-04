#' Add Prefix to Node Names
#'
#' Adds a prefix to node_list names and all input nodes.
#' Preserves previous prefixes unless rewrite_module argument is specified.
#'
#' @param mcmodule An mcmodule or a node_list object
#' @param prefix String to add as prefix of the new mcmodule mcnodes, defaults to mcmodule name
#' @param rewrite_module Name of a module to rewrite prefixes
#'
#' @return A mcmodule with new prefixes in node_list names
#'
#' @examples
#' print(names(imports_mcmodule$node_list))
#' imports_mcmodule_prefix<-purchase <- add_prefix(imports_mcmodule)
#' print(names(imports_mcmodule_prefix$node_list))
#' @export
add_prefix <- function(mcmodule, prefix = NULL, rewrite_module = NULL) {
  # Extract node list
  node_list <- mcmodule$node_list

  # Assign current mcmodule to nodes without one or when pipe was used and auto named to  "."
  for (i in 1:length(node_list)) {
    if (
      is.null(node_list[[i]][["module"]]) || node_list[[i]][["module"]] == "."
    ) {
      node_list[[i]][["module"]] <- deparse(substitute(mcmodule))
    }
  }

  # Get mcmodule structure
  info <- mcmodule_info(mcmodule)
  modules <- info$module_names
  exps <- unique(info$module_exp_data$exp)
  exp_and_modules <- c(modules, exps)

  # Get node names and modules
  node_names <- names(node_list)
  #node_module <- composition$module_names
  node_module <- unlist(sapply(node_list, "[[", "module"))
  node_exp <- unlist(sapply(node_list, "[[", "exp_name"))

  # Get inputs and their modules
  inputs <- unique(unlist(sapply(node_list, "[[", "inputs")))
  inputs_module <- unlist(sapply(node_list[inputs], "[[", "module"))
  inputs_exp <- unlist(sapply(node_list[inputs], "[[", "exp_name"))

  # Set default prefix if none provided
  if (is.null(prefix)) {
    prefix <- deparse(substitute(mcmodule))
  }

  # Handle module rewriting if specified
  if (!is.null(rewrite_module)) {
    # Rename module
    node_module[node_module %in% rewrite_module] <- prefix
    node_exp[node_exp %in% rewrite_module] <- prefix
    inputs_module[inputs_module %in% rewrite_module] <- prefix
    inputs_exp[inputs_exp %in% rewrite_module] <- prefix

    # Remove prefix
    node_names <- gsub(paste0(rewrite_module, "_"), "", node_names)
    names(node_module) <- gsub(
      paste0(rewrite_module, "_"),
      "",
      names(node_module)
    )
    names(node_exp) <- gsub(
      paste0(rewrite_module, "_"),
      "",
      names(node_exp)
    )
    names(inputs_module) <- gsub(
      paste0(rewrite_module, "_"),
      "",
      names(inputs_module)
    )
    names(inputs_exp) <- gsub(
      paste0(rewrite_module, "_"),
      "",
      names(inputs_exp)
    )
  }

  # Add prefix to node names
  node_prefix_index <- which(
    node_module[node_names] %in%
      exp_and_modules |
      node_exp[node_names] %in% exp_and_modules
  )

  node_names[node_prefix_index] <- paste0(
    prefix,
    "_",
    node_names[node_prefix_index]
  )

  # Add prefix to inputs
  inputs_prefix_index <- which(
    inputs_module[inputs] %in%
      exp_and_modules |
      inputs_exp[inputs] %in% exp_and_modules
  )

  new_inputs <- inputs

  new_inputs[inputs_prefix_index] <- paste0(
    prefix,
    "_",
    inputs[inputs_prefix_index]
  )

  # Remove duplicated prefixes
  node_names <- gsub(paste0(prefix, "_", prefix), prefix, node_names)
  new_inputs <- gsub(paste0(prefix, "_", prefix), prefix, new_inputs)

  names(new_inputs) <- inputs

  # Update node list inputs and prefix
  for (i in 1:length(node_list)) {
    if (
      node_list[[i]][["module"]] %in%
        modules ||
        (!is.null(node_list[[i]][["exp_name"]]) &&
          node_list[[i]][["exp_name"]] %in% exps)
    ) {
      old_inputs <- node_list[[i]][["inputs"]]
      node_list[[i]][["inputs"]] <- new_inputs[old_inputs]
      node_list[[i]][["prefix"]] <- prefix
    }
  }

  names(node_list) <- node_names

  # Return appropriate object type
  if (inherits(mcmodule, "mcmodule")) {
    mcmodule$node_list <- node_list
    mcmodule$prefix <- prefix
    return(mcmodule)
  } else {
    return(node_list)
  }
}
