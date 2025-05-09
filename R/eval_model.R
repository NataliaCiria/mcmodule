#' Evaluate and Process a Monte Carlo Model Expression
#'
#' Takes a model expression and evaluates it while creating a comprehensive node list
#' containing results and metadata. This function handles both single expressions and
#' lists of expressions, managing dependencies between nodes and previous modules.
#'
#' @param model_exp Model expression or list of expressions to evaluate
#' @param data Input data frame containing model parameters
#' @param param_names Named vector for parameter renaming (optional)
#' @param prev_mcmodule Previous module(s) for dependent calculations
#' @param match_prev Logical; whether to match dimensions with previous module
#' @param summary Logical; whether to calculate summary statistics
#' @param mctable Monte Carlo configuration table
#' @param data_keys List of key columns for each dataset
#' @param create_nodes Logical; whether to create new nodes from data
#' @param only_node_list Logical; return node list instead of full module
#'
#' @return Either an mcmodule object (containing data, expressions, and nodes)
#'         or just the node list if only_node_list=TRUE
#' @export
#'
#' @examples
#' # Basic usage with single expression
#' eval_model(
#'   model_exp = imports_exp,
#'   data = imports_data,
#'   mctable = imports_mctable,
#'   data_keys = imports_data_keys
#' )
eval_model <- function(model_exp, data, param_names = NULL,
                       prev_mcmodule = NULL, match_prev = FALSE,
                       summary = FALSE, mctable = set_mctable(),
                       data_keys = set_data_keys(), create_nodes = TRUE,
                       only_node_list = FALSE) {
  # Initialize  mcnodes if requested and data exists
  if (create_nodes & !is.null(data)) {
    create_mc_nodes(data = data, mctable = mctable)
  }

  data_name <- deparse(substitute(data))

  # Convert single expression to list format
  if (is.list(model_exp)) {
    model_exp_list <- model_exp
  } else {
    exp_name <- gsub("_exp", "", deparse(substitute(model_exp)))
    model_exp_list <- list(model_exp)
    names(model_exp_list) <- exp_name
  }

  node_list <- list()
  modules <- c()

  # Process each expression in the list
  for (i in 1:length(model_exp_list)) {
    model_exp_i <- model_exp_list[[i]]
    module <- names(model_exp_list)[[i]]

    # Get initial node list
    node_list_i <- get_node_list(model_exp_i,
      param_names = param_names,
      mctable = mctable, data_keys = data_keys
    )

    # Identify nodes requiring previous module data
    prev_nodes <- names(node_list_i)[grepl("prev_node", node_list_i)]
    prev_nodes <- prev_nodes[!prev_nodes %in% names(node_list)]

    # Process nodes requiring previous module data
    if (length(prev_nodes) > 0) {
      if (is.null(prev_mcmodule)) {
        stop(
          "prev_mcmodule for ", paste(prev_nodes, collapse = ", "),
          " needed but not provided"
        )
      } else {
        prev_mcmodule_list <- if (class(prev_mcmodule) == "mcmodule") {
          list(prev_mcmodule)
        } else {
          prev_mcmodule
        }

        # Previous modules
        for (j in 1:length(prev_mcmodule_list)) {
          prev_mcmodule_i <- prev_mcmodule_list[[j]]

          # Prefix matching for node names
          if (any(!prev_nodes %in% names(prev_mcmodule_i$node_list))) {
            prefixes <- unlist(sapply(prev_mcmodule_i$node_list, "[[", "prefix"))
            new_names <- sapply(names(prefixes), function(x) {
              gsub(paste0("^", prefixes[x], "_"), "", x)
            })

            original_names <- names(prefixes)
            names(prefixes) <- new_names

            prev_nodes_names <- prev_nodes
            prev_nodes <- ifelse(prev_nodes %in% original_names,
              prev_nodes, ifelse(is.na(prefixes[prev_nodes]), prev_nodes,
                paste0(prefixes[prev_nodes], "_", prev_nodes)
              )
            )
            names(prev_nodes) <- prev_nodes_names
            prev_param_names <- prev_nodes
          }

          # Get nodes from previous module
          prev_node_list_i <- get_mcmodule_nodes(prev_mcmodule_i,
            mc_names = prev_nodes
          )

          # Handle dimension matching
          if (length(prev_node_list_i) > 0) {
            dim_prev_nodes <- sapply(
              names(prev_node_list_i),
              function(node_name) dim(prev_node_list_i[[node_name]][["mcnode"]])[3]
            )
            node_name_max <- names(prev_node_list_i)[which.max(unlist(dim_prev_nodes))]
            agg_keys_max <- prev_node_list_i[[node_name_max]][["agg_keys"]]
          }

          # Process each previous node
          for (k in 1:length(prev_nodes)) {
            node_name <- prev_nodes[k]
            node_list_i[[node_name]] <- prev_node_list_i[[node_name]]

            if (match_prev) {
              if (is.null(prev_node_list_i[[node_name]][["agg_keys"]])) {
                match_prev_mcnode <- mc_match_data(prev_mcmodule, node_name, data)
                assign(node_name, match_prev_mcnode)
              } else {
                agg_keys <- prev_node_list_i[[node_name]][["agg_keys"]]

                if (!all(agg_keys_max == agg_keys)) {
                  stop("agg_keys do not match: ", agg_keys, " vs ", agg_keys_max)
                }

                match_agg_prev <- mc_match(
                  mcmodule = prev_mcmodule,
                  mc_name_x = node_name_max,
                  mc_name_y = node_name,
                  keys_names = agg_keys
                )

                match_prev_mcnode_max <- match_agg_prev[[1]]
                match_prev_mcnode <- match_agg_prev[[2]]
                data <- match_agg_prev[[3]]

                data_name <- paste0(node_name_max, "+", node_name)

                assign(node_name_max, match_prev_mcnode_max)
                assign(node_name, match_prev_mcnode)
              }
            }
          }
        }
      }
    }

    # Combine node lists
    node_list <- c(node_list, node_list_i)

    # Update parameter names
    new_param_names <- if (exists("prev_param_names")) {
      c(param_names, prev_param_names)
    } else {
      param_names
    }

    # Handle parameter renaming
    if (!is.null(new_param_names)) {
      for (j in 1:length(new_param_names)) {
        exp_name <- names(new_param_names)[j]
        param_name <- new_param_names[j]

        if (exists(param_name)) {
          assign(exp_name, get(param_name))
        } else if (!is.null(prev_mcmodule$node_list[[param_name]])) {
          assign(
            exp_name,
            prev_mcmodule$node_list[[param_name]][["mcnode"]]
          )
        }
      }
    }

    # Evaluate current expression
    eval(model_exp_i)
    message("\n", module, " evaluated")

    # Update node metadata
    for (j in 1:length(node_list)) {
      node_name <- names(node_list)[j]

      if (node_name %in% prev_nodes) next

      # Update input references
      inputs <- node_list[[node_name]][["inputs"]]
      node_list[[node_name]][["param"]] <- inputs

      inputs[inputs %in% names(new_param_names)] <-
        new_param_names[inputs[inputs %in% names(new_param_names)]]
      node_list[[node_name]][["inputs"]] <- inputs

      # Update keys for output nodes
      if (!is.null(prev_mcmodule) &
        node_list[[node_name]][["type"]] == "out_node") {
        keys_names <- unique(unlist(lapply(inputs, function(x) {
          node_list[[x]][["keys"]]
        })))
        node_list[[node_name]][["keys"]] <- keys_names
      }

      # Scalar to mcnode conversion
      mcnode <- get(node_name)
      if (!is.mcnode(mcnode) & is.numeric(mcnode)) {
        mcnode <- mcdata(mcnode, type = "0", nvariates = length(mcnode))
      }

      # Update node metadata
      node_list[[node_name]][["mcnode"]] <- mcnode
      node_list[[node_name]][["data_name"]] <- data_name
      node_list[[node_name]][["mc_name"]] <- node_name

      # Set module name
      if (length(node_list[[node_name]][["module"]]) == 0 ||
        node_list[[node_name]][["module"]] %in% "model_i") {
        node_list[[node_name]][["module"]] <- module
      }

      modules <- unique(c(modules, node_list[[node_name]][["module"]]))

      # Calculate summary statistics if requested
      if (summary & is.mcnode(mcnode)) {
        inputs_names <- node_list[[node_name]][["inputs"]]

        keys_names <- if (is.null(node_list[[node_name]][["agg_keys"]])) {
          node_list[[node_name]][["keys"]]
        } else {
          node_list[[node_name]][["agg_keys"]]
        }

        node_summary <- mc_summary(
          data = data, mcnode = mcnode,
          mc_name = node_name,
          keys_names = keys_names
        )

        node_list[[node_name]][["summary"]] <- node_summary
      }

      # Add scenario information if available
      if ("scenario_id" %in% names(data)) {
        node_list[[node_name]][["scenario"]] <- data$scenario_id
        if ("hg" %in% names(data)) {
          node_list[[node_name]][["hg"]] <- data$hg
        }
      }
    }
  }

  # Remove temporary previous nodes
  node_list <- node_list[!(sapply(node_list, "[[", "type") == "prev_node")]

  # Return results
  if (only_node_list) {
    return(node_list)
  } else {
    # Create and return complete module
    mcmodule <- list(
      data = list(data),
      model_exp = model_exp,
      node_list = node_list,
      modules = modules
    )

    names(mcmodule$data) <- data_name
    class(mcmodule) <- "mcmodule"

    message(
      "\nmcmodule created (expressions: ",
      paste(names(model_exp), collapse = ", "), ")"
    )

    return(mcmodule)
  }
}


#' Get Nodes from Monte Carlo Module
#'
#' Retrieves nodes from a Monte Carlo module and assigns them to the parent environment
#'
#' @param mcmodule An mcmodule or mcnode_list object
#' @param mc_names Optional vector of node names to retrieve
#'
#' @return A subset of the node list containing requested nodes
#' @export
get_mcmodule_nodes <- function(mcmodule, mc_names = NULL) {
  if (class(mcmodule) == "mcmodule") {
    node_list <- mcmodule$node_list
  } else if (class(mcmodule) == "mcnode_list") {
    node_list <- mcmodule
  } else {
    stop("mcmodule or mcnode_list object must be provided")
  }

  node_names <- names(node_list)
  node_names <- node_names[node_names %in% mc_names]

  if (length(node_names) > 0) {
    for (i in 1:length(node_names)) {
      node_name <- node_names[i]
      assign(node_name, node_list[[node_name]][["mcnode"]], envir = parent.frame())
    }
  }

  return(node_list[node_names])
}
