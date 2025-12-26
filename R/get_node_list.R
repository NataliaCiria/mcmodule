#' Create Node List from Model Expression
#'
#' Creates a list of nodes based on a given model expression, handling input,
#' output, and previous nodes with their properties and relationships.
#'
#' @param exp An R expression containing model calculations
#' @param param_names Optional named vector for parameter renaming
#' @param mctable Reference table for mcnodes, defaults to set_mctable()
#' @param data_keys Data structure and keys, defaults to set_data_keys()
#' @param keys Optional explicit keys for the input data (character vector)
#'
#' @return A list of class "mcnode_list" containing node information
get_node_list <- function(
  exp,
  param_names = NULL,
  mctable = set_mctable(),
  data_keys = set_data_keys(),
  keys = NULL
) {
  module <- gsub("_exp", "", deparse(substitute(exp)))

  # Initialize lists and vectors
  out_node_list <- list()
  all_nodes <- c()

  # Process output nodes from model exp
  for (i in 2:length(exp)) {
    node_name <- deparse(exp[[i]][[2]])
    node_exp <- paste0(deparse(exp[[i]][[3]]), collapse = "")
    
    # Parse expression tokens
    parse_info <- getParseData(parse(text = node_exp))
    inputs <- parse_info[parse_info$token == "SYMBOL", ]$text

    # If node expression contains mcstoc or mcdata, note node created in-expression
    # and validate/remove sampling function name from inputs.
    if (any(c("mcstoc","mcdata")%in%parse_info$text)) {
      out_node_list[[node_name]][["created_in_exp"]] <- TRUE

      # Stop if nvariates argument is found
      if ("nvariates" %in% parse_info$text) {
        stop(
          "Remove 'nvariates' argument from:\n   ",
          node_exp,
          "\nNumber of variates is determined automatically based on input data rows"
        )
      }

      # If mcstoc used with a 'func' named argument, remove the function name
      # from the inputs list so only data/symbol inputs remain.
      if ("mcstoc" %in% parse_info$text) {
        if ("func" %in% parse_info$text) {
          # parse_info$text rows are token stream; the function name appears two tokens after 'func'
          mc_func <- parse_info$text[which(parse_info$text == "func") + 2]
          inputs <- setdiff(inputs, mc_func)
        } else {
          mc_func <- inputs[1]
        }
        out_node_list[[node_name]][["mc_func"]] <- mc_func
      }
    }

    # Detect function that removes NAs inside the node expression
    if ("mcnode_na_rm" %in% parse_info$text) {
      out_node_list[[node_name]][["na_rm"]] <- TRUE
    }

    # Detect if any function calls are present in the expression
    if ("SYMBOL_FUNCTION_CALL" %in% parse_info$token) {
      out_node_list[[node_name]][["function_call"]] <- TRUE
    }

    # Collect node names and inputs
    all_nodes <- unique(c(all_nodes, node_name, inputs))

    # Set node type: numeric literal -> scalar; otherwise an output node that may depend on inputs
    out_node_list[[node_name]][["type"]] <-
      if (!grepl("[[:alpha:]]", node_exp) && !is.na(as.numeric(node_exp))) "scalar" else "out_node"

    out_node_list[[node_name]][["inputs"]] <- inputs
    out_node_list[[node_name]][["module"]] <- module
    out_node_list[[node_name]][["mc_name"]] <- node_name
  }

  # Rename parameters
  for (i in 1:length(all_nodes)) {
    all_nodes[i] <- if (all_nodes[i] %in% names(param_names)) {
      param_names[all_nodes[i]]
    } else {
      all_nodes[i]
    }
  }

  # Process input nodes
  in_node_list <- list()
  input_nodes <- all_nodes[all_nodes %in% as.character(mctable$mcnode)]

  # Build list of column names per dataset (safe when data_keys NULL or empty)
  if (is.null(data_keys) || length(data_keys) == 0) {
    all_inputs <- list()
  } else {
    all_inputs <- lapply(data_keys, function(x) {
      if (!is.null(x$cols)) {
        return(x$cols)
      }
      return(NULL)
    })
  }

  if (length(input_nodes) > 0) {
    for (i in 1:length(input_nodes)) {
      node_name <- input_nodes[[i]]
      mc_row <- mctable[mctable$mcnode == node_name, ]

      in_node_list[[node_name]][["type"]] <- "in_node"

      if (!is.na(mc_row$mc_func)) {
        in_node_list[[node_name]][["mc_func"]] <- as.character(mc_row$mc_func)
      }

      in_node_list[[node_name]][["description"]] <- as.character(
        mc_row$description
      )

      matched_dataset <- NULL

      # Process input columns and datasets
      for (dataset_name in names(all_inputs)) {
        if (is.null(mc_row$from_variable) || is.na(mc_row$from_variable)) {
          # Get matching input columns for current node
          pattern <- paste0("\\<", node_name, "(\\>|[^>]*\\>)")
          inputs_col <- all_inputs[[dataset_name]][grepl(
            pattern,
            all_inputs[[dataset_name]]
          )]
        } else {
          # Get matching input columns from transformed variable
          pattern <- paste0("\\<", mc_row$from_variable, "(\\>|[^>]*\\>)")
          inputs_col <- all_inputs[[dataset_name]][grepl(
            pattern,
            all_inputs[[dataset_name]]
          )]
          # Add transformation info to node list
          if (!is.null(mc_row$from_variable) && !is.na(mc_row$transformation)) {
            in_node_list[[node_name]][[
              "transformation"
            ]] <- mc_row$transformation
          }
        }

        # Update node list if matching inputs found
        if (length(inputs_col) > 0) {
          matched_dataset <- dataset_name
          in_node_list[[node_name]][["inputs_col"]] <- inputs_col
          in_node_list[[node_name]][["input_dataset"]] <- dataset_name

          # Determine dataset base keys from data_keys (if available)
          base_keys <- NULL
          if (
            !is.null(data_keys) &&
              dataset_name %in% names(data_keys) &&
              !is.null(data_keys[[dataset_name]][["keys"]])
          ) {
            base_keys <- data_keys[[dataset_name]][["keys"]]
          }

          # If explicit keys provided: merge with base_keys if base_keys exist; otherwise use explicit keys
          if (!is.null(keys)) {
            if (!is.character(keys)) {
              stop("keys must be a character vector")
            }
            final_keys <- if (!is.null(base_keys)) {
              unique(c(base_keys, keys))
            } else {
              keys
            }
          } else {
            final_keys <- base_keys
          }

          in_node_list[[node_name]][["keys"]] <- final_keys
        }
      }

      in_node_list[[node_name]][["module"]] <- module
      in_node_list[[node_name]][["mc_name"]] <- node_name

      # If no inputs_col matched (no dataset matched) but explicit keys were provided, assign them
      if (is.null(in_node_list[[node_name]][["keys"]]) && !is.null(keys)) {
        if (!is.character(keys)) {
          stop("keys must be a character vector")
        }
        in_node_list[[node_name]][["keys"]] <- keys
      }

      # Parameter renaming
      if (node_name %in% param_names) {
        node_name_exp <- names(param_names)[param_names %in% node_name]
        names(in_node_list)[
          names(in_node_list) %in% param_names
        ] <- node_name_exp
        all_nodes[all_nodes %in% param_names] <- node_name_exp
      }
    }
  }

  # Process previous nodes
  prev_node_list <- list()
  prev_nodes <- all_nodes[
    !all_nodes %in% c(names(in_node_list), names(out_node_list))
  ]

  if (length(prev_nodes) > 0) {
    for (i in 1:length(prev_nodes)) {
      node_name <- prev_nodes[i]
      is_fun <- if (exists(node_name)) is.function(get(node_name)) else FALSE
      if (!is_fun) {
        prev_node_list[[node_name]][["type"]] <- "prev_node"
      }
    }
  }

  # Combine all node lists (provisional list for key matching)
  node_list <- c(in_node_list, prev_node_list, out_node_list)

  # Process output node keys
  for (i in names(out_node_list)) {
    inputs <- node_list[[i]][["inputs"]]
    if (length(inputs) > 0) {
      keys_names <- unique(unlist(lapply(inputs, function(x) {
        node_list[[x]][["keys"]]
      })))
      if (length(keys_names) > 0) {
        node_list[[i]][["keys"]] <- keys_names
      }
    }
  }

  class(node_list) <- "mcnode_list"

  return(node_list)
}
