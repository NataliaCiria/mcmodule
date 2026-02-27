#' Filter an `mcnode` Object
#'
#' Filters an `mcnode` within an `mcmodule` based on conditions, similar to
#' `dplyr::filter()`. Can either create a new node in the `mcmodule` (when
#' both `mcmodule` and `name` are provided) or return a raw filtered `mcnode`
#' (when only `data` and `mcnode` are provided).
#'
#' @param mcmodule An mcmodule object containing the node to filter (default: NULL)
#' @param mc_name Character string specifying the name of the mcnode in the module (default: NULL)
#' @param ... Logical conditions to filter by (similar to dplyr::filter)
#' @param data Optional data frame containing the input data (default: NULL)
#' @param mcnode Optional mcnode object to filter directly (default: NULL)
#' @param name Optional name for the new filtered node when adding to mcmodule (default: NULL)
#' @param prefix Optional prefix for the filtered node name (default: NULL)
#' @param filter_suffix Suffix for the filtered node name (default: "filtered")
#' @param summary Logical; if TRUE, includes summary statistics (default: TRUE)
#'
#' @details
#' This function can be called in two ways:
#' 1. By providing an mcmodule and mc_name to create a new filtered node in the module
#' 2. By providing data and mcnode directly to return a raw filtered mcnode
#'
#' The filter conditions work on the data associated with the mcnode, and only
#' variates (rows) matching the conditions are kept in the resulting mcnode.
#'
#' @return Either:
#'   - An updated mcmodule with a new filtered node (when mcmodule and name are provided)
#'   - A raw filtered mcnode object (when only data and mcnode are provided)
#'
#' @examples
#' # Filter within an mcmodule and create new node
#' imports_mcmodule <- mc_filter(
#'   imports_mcmodule,
#'   "w_prev",
#'   origin == "Country A",
#'   name = "w_prev_countryA"
#' )
#'
#' # Filter and return raw mcnode (note: conditions before named args)
#' w_prev <- imports_mcmodule$node_list$w_prev$mcnode
#' w_prev_filtered <- mc_filter(
#'   origin == "Country A",
#'   data = imports_data,
#'   mcnode = w_prev
#' )
#'
#' # Multiple filter conditions
#' imports_mcmodule <- mc_filter(
#'   imports_mcmodule,
#'   "w_prev",
#'   origin == "Country A",
#'   pathogen == "virus",
#'   name = "w_prev_countryA_virus"
#' )
#' @export
mc_filter <- function(
  mcmodule = NULL,
  mc_name = NULL,
  ...,
  data = NULL,
  mcnode = NULL,
  name = NULL,
  prefix = NULL,
  filter_suffix = "filtered",
  summary = TRUE
) {
  eval_env <- parent.frame()
  mcmodule_expr <- substitute(mcmodule)
  filter_expr <- as.list(substitute(list(...)))[-1]

  if (length(filter_expr) == 0 && is.call(mcmodule_expr) && is.null(mc_name)) {
    filter_expr <- list(mcmodule_expr)
    mcmodule <- NULL
  } else {
    mcmodule <- eval(mcmodule_expr, eval_env)
  }

  if (length(filter_expr) == 0) {
    stop("At least one filter condition must be provided")
  }

  # Input validation
  if (!is.null(mcnode) && is.null(mc_name)) {
    mc_name <- deparse(substitute(mcnode))
  }

  return_mcmodule <- FALSE

  if (!is.null(mcmodule)) {
    module_name <- deparse(substitute(mcmodule))
    return_mcmodule <- TRUE

    if (is.null(mcnode)) {
      mcnode <- mcmodule$node_list[[mc_name]]$mcnode
    }

    if (!is.mcnode(mcnode)) {
      stop(sprintf("%s must be a mcnode present in %s", mc_name, module_name))
    }

    data_name <- mcmodule$node_list[[mc_name]]$data_name

    if (is.null(data)) {
      data <- mcmodule$data[[data_name]]
    }

    if (length(data_name) > 1) {
      message(
        "Multiple data names detected. Using first data_name for filtering."
      )
      data <- mcmodule$data[[data_name[1]]]
    }
  } else {
    if (is.null(data)) {
      stop("mcmodule or data must be provided")
    }
    if (is.null(mcnode)) {
      stop("mcnode must be provided when mcmodule is NULL")
    }
  }

  # Validate mcnode dimensions match data
  n_variates <- dim(mcnode)[3]
  if (n_variates != nrow(data)) {
    stop(sprintf(
      "Number of variates in mcnode (%d) does not match number of rows in data (%d)",
      n_variates,
      nrow(data)
    ))
  }

  # Apply filter to data
  data_filtered <- eval(
    as.call(c(quote(dplyr::filter), list(data), filter_expr)),
    eval_env
  )

  if (nrow(data_filtered) == 0) {
    warning("Filter conditions resulted in zero rows")
  }

  # Get indices of filtered rows without creating temporary columns
  filter_expr_combined <- if (length(filter_expr) == 1) {
    filter_expr[[1]]
  } else {
    Reduce(function(x, y) call("&", x, y), filter_expr)
  }

  filter_indices <- which(eval(filter_expr_combined, data, eval_env))

  if (length(filter_indices) == 0) {
    data_filtered <- data[0, , drop = FALSE]
  } else {
    data_filtered <- data[filter_indices, , drop = FALSE]
  }

  # Extract filtered variates from mcnode
  if (length(filter_indices) == 0) {
    mcnode_filtered <- mc2d::extractvar(mcnode, 1) -
      mc2d::extractvar(mcnode, 1)
  } else if (length(filter_indices) == 1) {
    mcnode_filtered <- mc2d::extractvar(mcnode, filter_indices)
  } else {
    mcnode_filtered <- mc2d::extractvar(mcnode, filter_indices[1])
    for (i in filter_indices[-1]) {
      mcnode_filtered <- mc2d::addvar(
        mcnode_filtered,
        mc2d::extractvar(mcnode, i)
      )
    }
  }

  # If not returning to mcmodule, just return the filtered mcnode
  if (!return_mcmodule) {
    return(mcnode_filtered)
  }

  # Generate name for filtered node
  filtered_mc_name <- if (!is.null(name)) {
    if (!is.null(filter_suffix) && filter_suffix != "") {
      paste0(name, "_", filter_suffix)
    } else {
      name
    }
  } else {
    if (!is.null(filter_suffix) && filter_suffix != "") {
      paste0(mc_name, "_", filter_suffix)
    } else {
      paste0(mc_name, "_flt")
    }
  }

  # Add prefix if provided
  if (!is.null(prefix) && prefix != "") {
    prefix <- paste0(sub("_$", "", prefix), "_")
    filtered_mc_name <- paste0(
      prefix,
      sub(paste0("^", prefix), "", filtered_mc_name)
    )
    prefix <- sub("_$", "", prefix)
  }

  # Get keys from original node
  keys_names <- if (!is.null(mcmodule$node_list[[mc_name]]$keys)) {
    mcmodule$node_list[[mc_name]]$keys
  } else {
    character(0)
  }

  # Create filter expression string for documentation
  filter_expr_str <- vapply(
    filter_expr,
    function(x) {
      paste(deparse(x), collapse = " ")
    },
    character(1)
  )
  filter_description <- paste(filter_expr_str, collapse = " & ")

  # Add new filtered node to module
  mcmodule$node_list[[filtered_mc_name]] <- list(
    mcnode = mcnode_filtered,
    type = "filter",
    param = mc_name,
    inputs = mc_name,
    description = sprintf("Filtered %s: %s", mc_name, filter_description),
    module = module_name,
    keys = keys_names,
    node_expression = sprintf("filter(%s, %s)", mc_name, filter_description),
    scenario = if ("scenario_id" %in% names(data_filtered)) {
      data_filtered$scenario_id
    } else {
      NULL
    },
    data_name = if (!is.null(mcmodule)) {
      mcmodule$node_list[[mc_name]][["data_name"]]
    } else {
      NULL
    },
    prefix = if (!is.null(prefix)) prefix else NULL,
    filter_conditions = filter_description
  )

  # Add summary if requested
  if (summary) {
    if (nrow(data_filtered) == 0) {
      mcmodule$node_list[[filtered_mc_name]][["summary"]] <- NULL
    } else {
      mcmodule$node_list[[filtered_mc_name]][["summary"]] <-
        mc_summary(
          mcmodule = mcmodule,
          data = data_filtered,
          mc_name = filtered_mc_name,
          keys_names = keys_names
        )
    }
  }

  return(mcmodule)
}
