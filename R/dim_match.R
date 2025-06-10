#' Get Monte Carlo Node Keys
#'
#' Extracts key columns from Monte Carlo node's associated data.
#'
#' @param mcmodule Monte Carlo module containing nodes and data
#' @param mc_name Name of the node to extract keys from
#' @param keys_names Vector of column names to extract (optional)
#'
#' @return Dataframe with scenario_id and requested key columns
#'
#' @examples
#' keys_df <- mc_keys(imports_mcmodule, "w_prev")
#'
#' @export
mc_keys <- function(mcmodule, mc_name, keys_names = NULL) {
  # Input validation
  if (!is.list(mcmodule) || is.null(mcmodule$node_list)) {
    stop("Invalid mcmodule structure")
  }
  if (!mc_name %in% names(mcmodule$node_list)) {
    stop("Node not found in module")
  }

  # Get the node from module
  node <- mcmodule$node_list[[mc_name]]

  # Determine keys to extract:
  # 1. Use provided keys_names if not NULL
  # 2. Otherwise use agg_keys if available
  # 3. Fall back to regular keys
  keys_names <- if (!is.null(keys_names)) {
    keys_names
  } else if (!is.null(node[["agg_keys"]])) {
    node[["agg_keys"]]
  } else {
    node[["keys"]]
  }

  # Get data based on node type (aggregated or regular)
  data <- if (!is.null(node[["agg_keys"]])) {
    node[["summary"]]
  } else {
    mcmodule$data[[node[["data_name"]]]]
  }

  # Validate all requested keys exist
  missing_keys <- setdiff(keys_names, names(data))
  if (length(missing_keys) > 0) {
    stop(sprintf(
      "Columns %s not found in %s data",
      paste(missing_keys, collapse = ", "),
      mc_name
    ))
  }

  # Add scenario_id column if missing
  if (!"scenario_id" %in% names(data)) {
    data$scenario_id <- "0"
  }

  # Check for duplicates in baseline scenario
  if (any(duplicated(data[data$scenario_id == "0", keys_names]))) {
    message(sprintf("Duplicated keys in scenario 0 for %s", mc_name))
  }

  # Return only requested columns
  data[unique(c("scenario_id", keys_names))]
}

#' Match Monte Carlo Nodes
#'
#' Matches two mcnodes by:
#' 1. Group matching - Align nodes with same scenarios but different group order
#' 2. Scenario matching - Align nodes with same groups but different scenarios
#' 3. Null matching - Add missing groups across different scenarios
#'
#' @param mcmodule Monte Carlo module
#' @param mc_name_x First node name
#' @param mc_name_y Second node name
#' @param keys_names Names of key columns
#' @return List containing matched nodes and combined keys (keys_xy)
#' @export
mc_match <- function(mcmodule, mc_name_x, mc_name_y, keys_names = NULL) {
  # Check if mcnodes are in mcmodule
  missing_nodes <- c(mc_name_x, mc_name_y)[!c(mc_name_x, mc_name_y) %in% names(mcmodule$node_list)]
  if (length(missing_nodes) > 0) {
    stop(paste("Nodes", paste(missing_nodes, collapse = ", "), "not found in", deparse(substitute(mcmodule))))
  }

  # Get nodes
  mcnode_x <- mcmodule$node_list[[mc_name_x]][["mcnode"]]
  mcnode_y <- mcmodule$node_list[[mc_name_y]][["mcnode"]]

  # Get nodes data name
  data_name_x <- mcmodule$node_list[[mc_name_x]][["data_name"]]
  data_name_y <- mcmodule$node_list[[mc_name_y]][["data_name"]]

  # Remove scenario_id from keys
  keys_names <- keys_names[!keys_names == "scenario_id"]

  # Get keys dataframes for x and y
  keys_x <- mc_keys(mcmodule, mc_name_x, keys_names)
  keys_y <- mc_keys(mcmodule, mc_name_y, keys_names)

  # If nodes do not have the same keys but both nodes come from the same data, keys are inferred from data
  if(data_name_x==data_name_y&&
     nrow(keys_x) == nrow(keys_y)&&
     ncol(keys_x) != ncol(keys_y)&&
     all(keys_x[intersect(names(keys_x),names(keys_y))]==keys_y[intersect(names(keys_x),names(keys_y))])){

    # Find keys that are only pressent in one of the mcnodes
    keys_x_only<-setdiff(names(keys_x),names(keys_y))
    keys_y_only<-setdiff(names(keys_y),names(keys_x))

    if(length(keys_x_only)>0) message("Keys infered from ",c(data_name_x)," for ",mc_name_x,": ",
      paste0(keys_x_only,sep=", "))

    if(length(keys_y_only)>0) message("Keys infered from ",c(data_name_y)," for ",mc_name_x,": ",
                                      paste0(keys_y_only,sep=", "))

    # Add the same keys to both mcnodes
    keys_x<-cbind(keys_x[intersect(names(keys_x),names(keys_y))],
                  keys_x[keys_x_only],
                  keys_y[keys_y_only])

    keys_y<-cbind(keys_y[intersect(names(keys_x),names(keys_y))],
                  keys_x[keys_x_only],
                  keys_y[keys_y_only])

    # Return nodes as they are if they already match
    message(
      mc_name_x, " and ", mc_name_y, " already match, dim: [",
      paste(dim(mcnode_x), collapse = ", "), "]"
    )

    return(list(
      mcnode_x_match = mcnode_x,
      mcnode_y_match = mcnode_y,
      keys_xy = keys_match(keys_x, keys_y, keys_names)$xy
    ))

  }

  # Match keys
  keys_list <- keys_match(keys_x, keys_y, keys_names)
  keys_x <- keys_list$x
  keys_y <- keys_list$y
  keys_xy <- keys_list$xy


  # Match nodes
  null_x <- 0
  null_y <- 0

  # Process X node
  for (i in 1:nrow(keys_xy)) {
    g_row_x_i <- keys_xy$g_row.x[i]

    if (keys_xy$g_id[i] %in% keys_x$g_id) {
      mc_i <- extractvar(mcnode_x, g_row_x_i)
    } else {
      mc_i <- extractvar(mcnode_x, 1) - extractvar(mcnode_x, 1)
      null_x <- null_x + 1
    }

    if (i == 1) {
      mcnode_x_match <- mc_i
    } else {
      mcnode_x_match <- addvar(mcnode_x_match, mc_i)
    }
  }

  # Process Y node
  for (i in 1:nrow(keys_xy)) {
    g_row_y_i <- keys_xy$g_row.y[i]

    if (keys_xy$g_id[i] %in% keys_y$g_id) {
      mc_i <- extractvar(mcnode_y, g_row_y_i)
    } else {
      mc_i <- extractvar(mcnode_y, 1) - extractvar(mcnode_y, 1)
      null_y <- null_y + 1
    }

    if (i == 1) {
      mcnode_y_match <- mc_i
    } else {
      mcnode_y_match <- addvar(mcnode_y_match, mc_i)
    }
  }

  # Log results
  message(
    mc_name_x, " prev dim: [", paste(dim(mcnode_x), collapse = ", "),
    "], new dim: [", paste(dim(mcnode_x_match), collapse = ", "),
    "], ", null_x, " null matches"
  )

  message(
    mc_name_y, " prev dim: [", paste(dim(mcnode_y), collapse = ", "),
    "], new dim: [", paste(dim(mcnode_y_match), collapse = ", "),
    "], ", null_y, " null matches"
  )

  # Return results
  result <- list(mcnode_x_match, mcnode_y_match, keys_xy)
  names(result) <- c(
    paste0(mc_name_x, "_match"),
    paste0(mc_name_y, "_match"),
    "keys_xy"
  )

  return(result)
}

#' Match Monte Carlo Node with other data frame
#'
#' Matches an mcnode with a data frame by:
#' 1. Group matching - Same scenarios but different group order
#' 2. Scenario matching - Same groups but different scenarios
#' 3. Null matching - Add missing groups across different scenarios
#'
#' @param mcmodule Monte Carlo module
#' @param mc_name Node name
#' @param data Data frame containing keys to match with
#' @param keys_names Names of key columns
#' @return List containing matched node, matched data and combined keys (keys_xy)
#' @export
mc_match_data <- function(mcmodule, mc_name, data, keys_names = NULL) {
  # Check if mcnodes are in mcmodule
  if (!mc_name%in% names(mcmodule$node_list)) {
    stop(paste("Nodes", mc_name, "not found in", deparse(substitute(mcmodule))))
  }

  # Get node
  mcnode_x <- mcmodule$node_list[[mc_name]][["mcnode"]]

  # Get nodes data name
  data_name_x <- mcmodule$node_list[[mc_name]][["data_name"]]
  data_name_y <- deparse(substitute(data))

  # Remove scenario_id from keys
  keys_names <- keys_names[!keys_names == "scenario_id"]

  # Get keys dataframes for x and y
  keys_x <- mc_keys(mcmodule, mc_name, keys_names)
  keys_data<-intersect(names(keys_x),names(data))
  keys_y <- data[keys_data]

  # If nodes do not have the same keys but both nodes come from the same data, keys are inferred from data
  if(data_name_x==data_name_y&&
     nrow(keys_x) == nrow(keys_y)&&
     ncol(keys_x) != ncol(keys_y)&&
     all(keys_x[intersect(names(keys_x),names(keys_y))]==keys_y[intersect(names(keys_x),names(keys_y))])){

    # Return nodes as they are if they already match
    message(
      mc_name, " and ", data_name_y, " already match, dim: [",
      paste(dim(mcnode_x), collapse = ", "), "]"
    )

    return(list(
      mcnode_match = mcnode_x,
      data_match = data,
      keys_xy = keys_match(keys_x, keys_y, keys_names)$xy
    ))

  }

  # Match keys
  keys_list <- keys_match(keys_x, keys_y, keys_names)
  keys_x <- keys_list$x
  keys_y <- keys_list$y
  keys_xy <- keys_list$xy


  # Match nodes
  null_x <- 0
  null_y <- 0

  # Process X node
  for (i in 1:nrow(keys_xy)) {
    g_row_x_i <- keys_xy$g_row.x[i]

    if (keys_xy$g_id[i] %in% keys_x$g_id) {
      mc_i <- extractvar(mcnode_x, g_row_x_i)
    } else {
      mc_i <- extractvar(mcnode_x, 1) - extractvar(mcnode_x, 1)
      null_x <- null_x + 1
    }

    if (i == 1) {
      mcnode_x_match <- mc_i
    } else {
      mcnode_x_match <- addvar(mcnode_x_match, mc_i)
    }
  }
  # Process data
  for (i in 1:nrow(keys_xy)) {
    g_row_y_i <- keys_xy$g_row.y[i]

    if (keys_xy$g_id[i] %in% keys_y$g_id) {
      row_i <- data[g_row_y_i,]
    } else {
      row_i <- keys_xy[i,keys_data]
      null_y <- null_y + 1
    }

    if (i == 1) {
      data_match <- row_i
    } else {
      data_match <- bind_rows(data_match, row_i)
    }
  }

  # Log results
  message(
    mc_name, " prev dim: [", paste(dim(mcnode_x), collapse = ", "),
    "], new dim: [", paste(dim(mcnode_x_match), collapse = ", "),
    "], ", null_x, " null matches"
  )

  message(
    data_name_y, " prev dim: [", paste(dim(data), collapse = ", "),
    "], new dim: [", paste(dim(data_match), collapse = ", "),
    "], ", null_y, " null matches"
  )


  # Return results
  result <- list(mcnode_x_match, data_match, keys_xy)
  names(result) <- c(
    paste0(mc_name, "_match"),
    paste0(data_name_y, "_match"),
    "keys_xy"
  )

  return(result)
}

#' Match Datasets With Differing Scenarios
#'
#' Matches datasets by group and preserves baseline scenarios (scenario_id=0) when scenarios differ between them.
#'
#' @param x First dataset to match
#' @param y Second dataset to match
#' @param by Grouping variable(s) to match on, defaults to "hg" (homogeneous groups)
#' @return List containing matched datasets with aligned scenario IDs:
#'   - First element: matched version of dataset x
#'   - Second element: matched version of dataset y
#' @examples
#' x <- data.frame(
#'   category = c("a", "b", "a", "b"),
#'   scenario_id = c(0, 0, 1, 1),
#'   hg = c(1, 2, 1, 2),
#'   value = 1:4
#' )
#'
#' y <- data.frame(
#'   category = c("a", "b", "a", "b"),
#'   scenario_id = c(0, 0, 2, 2),
#'   hg = c(1, 2, 1, 2),
#'   value = 5:8
#' )
#'
#' # Automatic matching
#' result <- wif_match(x, y)
#'
#' @export
wif_match <- function(x, y, by = "hg") {
  # Match keys between datasets
  list_xy <- keys_match(x, y, by)

  # Find any unmatched groups in both datasets
  null_x <- unlist(unique(list_xy$xy[is.na(list_xy$xy$g_row.x), by]))
  null_y <- unlist(unique(list_xy$xy[is.na(list_xy$xy$g_row.y), by]))

  # Format error messages for unmatched groups
  w_null_x <- paste(names(null_x), null_x, sep = " ", collapse = ", ")
  w_null_y <- paste(names(null_y), null_y, sep = " ", collapse = ", ")

  # Stop if any groups couldn't be matched
  if (any(is.na(list_xy$xy$g_row.x)) || any(is.na(list_xy$xy$g_row.y))) {
    error_msg <- character()
    if (any(is.na(list_xy$xy$g_row.x))) error_msg <- c(error_msg, paste("In x:", w_null_x))
    if (any(is.na(list_xy$xy$g_row.y))) error_msg <- c(error_msg, paste("In y:", w_null_y))
    stop(paste("Groups not found:", paste(error_msg, collapse = "; ")))
  }

  # Create matched versions of both datasets
  new_x <- y[list_xy$xy$g_row.x, ] %>%
    mutate(scenario_id = list_xy$xy$scenario_id)
  rownames(new_x) <- NULL

  new_y <- y[list_xy$xy$g_row.y, ] %>%
    mutate(scenario_id = list_xy$xy$scenario_id)
  rownames(new_y) <- NULL

  # Count homogeneous groups for logging
  n_hg_x <- ifelse("hg" %in% names(x), max(x$hg), "no")
  n_hg_y <- ifelse("hg" %in% names(y), max(y$hg), "no")

  # Log matching results
  message(
    "From ", nrow(x), " rows (", n_hg_x, " hg) and ",
    nrow(y), " rows (", n_hg_y, " hg), to ",
    nrow(new_x), " rows (", max(new_x$hg), " hg)\n"
  )

  # Return list with matched datasets
  list_new_xy <- list(new_x, new_y)
  names(list_new_xy) <- c(deparse(substitute(x)), deparse(substitute(y)))

  return(list_new_xy)
}
