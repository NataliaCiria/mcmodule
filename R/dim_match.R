#' Add Group IDs to Data Frames
#'
#' @param x First dataset
#' @param by Grouping variables
#' @param y Optional second dataset
#' @return Dataframe or list of dataframes with added group IDs
#' @import dplyr
#' @export
#' @examples
#' add_group_id(imports_data, by = c("pathogen ", "origin"))
add_group_id <- function(x, y = NULL, by = NULL) {
  if (!is.null(y)) {
    if (is.null(by)) {
      # Get categorical variables for each dataframe
      cat_x <- names(x)[sapply(x, function(col) is.character(col) | is.factor(col))]
      cat_y <- names(y)[sapply(y, function(col) is.character(col) | is.factor(col))]

      # Find intersection of categorical variables
      by <- intersect(cat_x, cat_y)
      message("Group by: ", paste(by, collapse = ", "))
    }

    if (!all(by %in% names(x))) {
      stop(paste0(by[by %in% names(x)], " columns not found in ", deparse(substitute(x))))
    }
    if (!all(by %in% names(y))) {
      stop(paste0(by[by %in% names(y)], " columns not found in ", deparse(substitute(y))))
    }

    x$df <- "x"
    y$df <- "y"

    xy <- dplyr::bind_rows(x[c(by, "df")], y[c(by, "df")]) %>%
      dplyr::mutate(
        g_id = NULL,
        g_row = NULL
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::mutate(g_id = dplyr::cur_group_id())

    x <- xy %>%
      dplyr::filter(df == "x") %>%
      dplyr::bind_cols(x[!names(x) %in% c(by, "df", "g_id", "g_row")]) %>%
      dplyr::mutate(
        df = NULL,
        g_row = dplyr::cur_group_rows()
      ) %>%
      dplyr::relocate(g_id, g_row) %>%
      dplyr::ungroup()

    y <- xy %>%
      dplyr::filter(df == "y") %>%
      dplyr::bind_cols(y[!names(y) %in% c(by, "df", "g_id", "g_row")]) %>%
      dplyr::mutate(
        df = NULL,
        g_row = dplyr::cur_group_rows()
      ) %>%
      dplyr::relocate(g_id, g_row) %>%
      dplyr::ungroup()

    return(list(x = x, y = y))
  } else {
    x <- x %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::mutate(
        g_id = dplyr::cur_group_id(),
        g_row = dplyr::cur_group_rows()
      ) %>%
      dplyr::relocate(g_id, g_row) %>%
      dplyr::ungroup()

    return(x)
  }
}

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
    warning(sprintf("Duplicated keys in scenario 0 for %s", mc_name))
  }

  # Return only requested columns
  data[unique(c("scenario_id", keys_names))]
}


#' Match and align keys between two datasets
#'
#' @param x First dataset containing keys to match
#' @param y Second dataset containing keys to match
#' @param keys_names Names of columns to use as matching keys. If NULL, uses common columns
#' @return List containing:
#'   \item{x}{First dataset with group IDs}
#'   \item{y}{Second dataset with group IDs}
#'   \item{xy}{Matched datasets with aligned group and scenario IDs}
#' @import dplyr
#' @examples
#' x <- data.frame(type = 1:2, scenario_id = c(0, 1))
#' y <- data.frame(type = 1:2, scenario_id = c(0, 2))
#' keys_match(x, y, keys_names = "type")
keys_match <- function(x, y, keys_names = NULL) {
  # Add common group ids
  keys_list <- add_group_id(x, y, keys_names)
  keys_x <- keys_list$x
  keys_y <- keys_list$y

  # Define keys_names if not provided
  if (is.null(keys_names)) {
    keys_names <- unique(intersect(names(keys_x), names(keys_y)))
    keys_names <- keys_names[!keys_names %in% c("g_id", "g_row", "scenario_id")]
  }

  # Group and scenario matching
  keys_xy <- keys_x %>%
    dplyr::full_join(keys_y, by = c("g_id", "scenario_id", keys_names)) %>%
    dplyr::relocate("g_id", "scenario_id", dplyr::all_of(keys_names))

  # Get group ids for baseline scenario (scenario_id = 0)
  keys_xy_0 <- keys_xy %>%
    dplyr::full_join(keys_y, by = c("g_id", "scenario_id", keys_names)) %>%
    dplyr::filter(scenario_id == 0) %>%
    dplyr::transmute(
      g_id,
      g_row.x_0 = g_row.x,
      g_row.y_0 = g_row.y
    )

  # Fill in missing values using baseline scenario
  keys_xy <- keys_xy %>%
    dplyr::left_join(keys_xy_0, by = "g_id") %>%
    dplyr::mutate(
      g_row.x = ifelse(is.na(g_row.x), g_row.x_0, g_row.x),
      g_row.x_0 = NULL,
      g_row.y = ifelse(is.na(g_row.y), g_row.y_0, g_row.y),
      g_row.y_0 = NULL
    )

  return(list(
    x = keys_x,
    y = keys_y,
    xy = keys_xy
  ))
}

#' Match Monte Carlo Nodes
#'
#' Matches two mc_nodes by:
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

  # Remove scenario_id from keys
  keys_names <- keys_names[!keys_names == "scenario_id"]

  # Get keys dataframes for x and y
  keys_x <- mc_keys(mcmodule, mc_name_x, keys_names)
  keys_y <- mc_keys(mcmodule, mc_name_y, keys_names)


  # Match keys
  keys_list <- keys_match(keys_x, keys_y, keys_names)
  keys_x <- keys_list$x
  keys_y <- keys_list$y
  keys_xy <- keys_list$xy


  # Return nodes as they are if they already match
  if (nrow(keys_x) == nrow(keys_y) &&
    all(keys_x[c("g_id", "scenario_id")] == keys_y[c("g_id", "scenario_id")])) {
    message(
      mc_name_x, " and ", mc_name_y, " already match, dim: [",
      paste(dim(mcnode_x), collapse = ", "), "]"
    )

    return(list(
      mcnode_x_match = mcnode_x,
      mcnode_y_match = mcnode_y,
      keys_xy = keys_xy
    ))
  }

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

#' Match Monte Carlo Datasets With Differing Scenarios
#'
#' Matches datasets by group and preserves baseline scenarios (scenario_id=0) when scenarios differ between them.
#'
#' @param x First dataset to match
#' @param y Second dataset to match
#' @param by Grouping variable(s) to match on, defaults to "hg" (homogeneous groups)
#' @return List containing matched datasets with aligned scenario IDs:
#'   - First element: matched version of dataset x
#'   - Second element: matched version of dataset y
#' @export
#' @examples
#' x <- data.frame(hg = 1:2, val = c("a", "b"), scenario_id = c(0, 1))
#' y <- data.frame(hg = 1:2, val = c("c", "d"), scenario_id = c(0, 2))
#' matched <- wif_match(x, y)
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
