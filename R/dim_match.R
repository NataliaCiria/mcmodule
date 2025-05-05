#' Add Group IDs to Data Frames
#'
#' @param x First dataset
#' @param by Grouping variables
#' @param y Optional second dataset
#' @return Dataframe or list of dataframes with added group IDs
#' @import dplyr
#' @importFrom dplyr mutate filter group_by_at ungroup cur_group_id cur_group_rows relocate vars any_of
#' @export
#' @examples
#' add_group_id(purchase_data, by=c("animal_category", "pathogen", "mov_id", "farm_id"))
add_group_id <- function(x, y=NULL, by=NULL) {
  if(!is.null(y)) {
    if(is.null(by)) {
      # Get categorical variables for each dataframe
      cat_x <- names(x)[sapply(x, function(col) is.character(col) | is.factor(col))]
      cat_y <- names(y)[sapply(y, function(col) is.character(col) | is.factor(col))]

      # Find intersection of categorical variables
      by <- intersect(cat_x, cat_y)
      message("Group by: ", paste(by, collapse=", "))
    }

    if(!all(by %in% names(x))) {
      stop(paste0(by[by %in% names(x)], " columns not found in ", deparse(substitute(x))))
    }
    if(!all(by %in% names(y))) {
      stop(paste0(by[by %in% names(y)], " columns not found in ", deparse(substitute(y))))
    }

    x$df <- "x"
    y$df <- "y"

    xy <- rbind(x[c(by,"df")], y[c(by,"df")]) %>%
      mutate(g_id=NULL,
             g_row=NULL) %>%
      group_by_at(vars(any_of(by))) %>%
      mutate(g_id=cur_group_id())

    x <- xy %>%
      filter(df=="x") %>%
      cbind(x[!names(x) %in% c(by,"df","g_id","g_row")]) %>%
      mutate(df=NULL,
             g_row=cur_group_rows()) %>%
      relocate(g_id, g_row) %>%
      ungroup()

    y <- xy %>%
      filter(df=="y") %>%
      cbind(y[!names(y) %in% c(by,"df","g_id","g_row")]) %>%
      mutate(df=NULL,
             g_row=cur_group_rows()) %>%
      relocate(g_id, g_row) %>%
      ungroup()

    return(list(x=x, y=y))

  } else {
    x <- x %>%
      group_by_at(vars(any_of(by))) %>%
      mutate(g_id=cur_group_id(),
             g_row=cur_group_rows()) %>%
      relocate(g_id,g_row) %>%
      ungroup()

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
#' \dontrun{
#' keys_df <- mc_keys(mc_module, "node1", c("key1", "key2"))
#' }
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
    stop(sprintf("Columns %s not found in %s data",
                 paste(missing_keys, collapse = ", "),
                 mc_name))
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
#' @examples
#' mc_match(mcmodule = intro,
#'              mc_name_x="no_purchase_inf_agg",
#'              mc_name_y="b_entry_agg")
mc_match <- function(mcmodule, mc_name_x, mc_name_y, keys_names=NULL) {

  # Check if mcnodes are in mcmodule
  missing_nodes<-c(mc_name_x,mc_name_y)[!c(mc_name_x,mc_name_y)%in%names(mcmodule$node_list)]
  if (length(missing_nodes) > 0) {
    stop(paste("Nodes",paste(missing_nodes, collapse = ", "),"not found in",deparse(substitute(mcmodule))))
  }

  # Get nodes
  mcnode_x <- mcmodule$node_list[[mc_name_x]][["mcnode"]]
  mcnode_y <- mcmodule$node_list[[mc_name_y]][["mcnode"]]

  # Remove scenario_id from keys
  keys_names <- keys_names[!keys_names=="scenario_id"]

  # Get keys dataframes for x and y
  keys_x <- mc_keys (mcmodule, mc_name_x, keys_names)
  keys_y <- mc_keys (mcmodule, mc_name_y, keys_names)

  # Add common group ids
  keys_list <- add_group_id(x=keys_x, y=keys_y, by=keys_names)
  keys_x <- keys_list$x
  keys_y <- keys_list$y

  # Return nodes as they are if they already match
  if (nrow(keys_x) == nrow(keys_y) &&
      all(keys_x[c(keys_names, "scenario_id")] == keys_y[c(keys_names, "scenario_id")])) {
    message(mc_name_x, " and ", mc_name_y, " already match, dim: [",
            paste(dim(mcnode_x), collapse=", "), "]")

    return(list(
      mcnode_x_match = mcnode_x,
      mcnode_y_match = mcnode_y,
      index = keys_x[c(keys_names, "scenario_id")]
    ))
  }

  # Group and scenario matching
  keys_xy<-keys_x%>%
    full_join(keys_y, by=c("g_id", "scenario_id", keys_names))%>%
    relocate("g_id", "scenario_id", all_of(keys_names))

  keys_xy_0<-keys_xy%>%
    full_join(keys_y, by=c("g_id", "scenario_id", keys_names))%>%
    filter(scenario_id==0)%>%
    transmute(
      g_id,
      g_row.x_0=g_row.x,
      g_row.y_0=g_row.y)

  keys_xy<-keys_xy%>%
    left_join(keys_xy_0, by="g_id")%>%
    mutate(
      g_row.x=ifelse(is.na(g_row.x),g_row.x_0,g_row.x),
      g_row.x_0=NULL,
      g_row.y=ifelse(is.na(g_row.y),g_row.y_0,g_row.y),
      g_row.y_0=NULL)


  # Match nodes
  null_x <- 0
  null_y <- 0

  # Process X node
  for(i in 1:nrow(keys_xy)) {
    g_row_x_i <- keys_xy$g_row.x[i]

    if(keys_xy$g_id[i] %in% keys_x$g_id) {
      mc_i <- extractvar(mcnode_x,g_row_x_i)
    } else {
      mc_i <- extractvar(mcnode_x, 1)-extractvar(mcnode_x, 1)
      null_x <- null_x+1
    }

    if(i==1) {
      mcnode_x_match <- mc_i
    } else {
      mcnode_x_match <- addvar(mcnode_x_match,mc_i)
    }

  }

  # Process Y node
  for(i in 1:nrow(keys_xy)) {
    g_row_y_i <- keys_xy$g_row.y[i]

    if(keys_xy$g_id[i] %in% keys_y$g_id) {
      mc_i <- extractvar(mcnode_y,g_row_y_i)
    } else {
      mc_i <- extractvar(mcnode_y, 1)-extractvar(mcnode_y, 1)
      null_y <- null_y+1
    }

    if(i==1) {
      mcnode_y_match <- mc_i
    } else {
      mcnode_y_match <- addvar(mcnode_y_match,mc_i)
    }
  }

  # Log results
  message(mc_name_x, " prev dim: [", paste(dim(mcnode_x), collapse=", "),
          "], new dim: [", paste(dim(mcnode_x_match), collapse=", "),
          "], ", null_x, " null matches")

  message(mc_name_y, " prev dim: [", paste(dim(mcnode_y), collapse=", "),
          "], new dim: [", paste(dim(mcnode_y_match), collapse=", "),
          "], ", null_y, " null matches")

  # Return results
  result <- list(mcnode_x_match, mcnode_y_match, keys_xy)
  names(result) <- c(paste0(mc_name_x,"_match"),
                     paste0(mc_name_y,"_match"),
                     "keys_xy")

  return(result)
}

#' Match Monte Carlo Datasets With Differing Scenarios
#'
#' @param x First dataset
#' @param y Second dataset
#' @param by Optional grouping variables
#' @param cross_scenarios Cross join all scenarios
#' @return Matched dataset
#' @export
#' @examples
#' wif_match(dataset1, dataset2, by=c("category", "group"))
wif_match <- function(x, y, by=NULL, cross_scenarios=FALSE) {
  if(is.null(by)) {
    message("Match by homogeneous groups (hg)")
    by<-"hg"

    if(!"hg" %in% names(y) & !"hg" %in% names(x)) {
      stop("No homogeneous group index (hg) found and no match 'by' parameter specified")
    }

    if("hg" %in% names(x)) message("- Using x ", max(x$hg)," hg")

    if("hg" %in% names(y)) message("- Using y ", max(y$hg)," hg")


    if((!"hg" %in% names(y) | !"hg" %in% names(x))&
       !sum(x$scenario_id=="0")==sum(y$scenario_id=="0")) {
      stop("Differing number of rows in x and y. Specify variables to match in 'by' parameter.")
    }

    if(!"hg" %in% names(x) & "hg" %in% names(y)) {
      x$hg <- y$hg[y$scenario_id=="0"]
      message("- x hg copied from y")
    }
    if(!"hg" %in% names(y) & "hg" %in% names(x)) {
      y$hg <- x$hg[x$scenario_id=="0"]
      message("- y hg copied from x")

    }
  }else{
    message("Group id by ", paste0(by,collapse=", "))
  }

  new_x<-y %>%
    add_group_id(by=by) %>%
    select(g_id, true_scenario=scenario_id, true_hg=hg) %>%
    left_join(add_group_id(x=x, by=by)) %>%
    mutate(both_current=(true_scenario=="0"&scenario_id=="0"),
           any_current=(true_scenario=="0"|scenario_id=="0"),
           same_scenario=true_scenario==scenario_id,
           scenario_id=true_scenario,
           hg=true_hg)%>%
    relocate(hg, g_id, g_row, scenario_id)

  if(cross_scenarios){
    new_x<-new_x %>%
      mutate(scenario_id=paste(scenario_id, " + ", true_scenario))
    message("\nWIF scenarios combined")
  }else{
    # Keep rows where both scenarios have same value
    new_x_same_scenarios<-new_x %>%
      filter(same_scenario)

    # Keep rows where values don't match and one is 0
    new_x_current_scenarios <- new_x %>%
      filter(!same_scenario&any_current&!true_scenario%in%new_x_same_scenarios$true_scenario)

    new_x<-bind_rows(new_x_same_scenarios,
                     new_x_current_scenarios)%>%
      select(-any_current,-both_current,-true_hg,-true_scenario)

    message("\nWIF scenarios matched with current scenarios")
  }

  n_hg_x<-ifelse("hg"%in%names(x),max(x$hg),"no")
  n_hg_y<-ifelse("hg"%in%names(y),max(y$hg),"no")

  message("From ", nrow(x), " rows (",n_hg_x," hg) and ", nrow(y), " rows (",n_hg_y," hg), to ", nrow(new_x), " rows (", max(new_x$hg), " hg)\n")
  return(new_x)
}


#' Match Monte Carlo Nodes
#'
#' Matches monte carlo nodes with differing dimensions assuming the new variates
#' are equal for scenario 0 for the smallest node
#'
#' @param mcmodule Monte Carlo module
#' @param mc_name Name of the Monte Carlo node
#' @param data Input data
#' @return Modified Monte Carlo node
#' @export
#' @examples
#' mc_match_data(mcmodule = purchase, mc_name="fattening_b_indir_contact_all", data=quarantine_data)
mc_match_data <- function(mcmodule, mc_name, data) {
  # Get previous node data
  prenode_hg <- mcmodule$node_list[[mc_name]][["hg"]]
  prenode_scenario <- mcmodule$node_list[[mc_name]][["scenario"]]
  match_prev_mcnode <- mcmodule$node_list[[mc_name]][["mcnode"]]

  if(is.null(match_prev_mcnode)) {
    stop(mc_name, " is null")
  }

  # Handle scalar to mcnode conversion
  if(!is.mcnode(match_prev_mcnode) & is.numeric(match_prev_mcnode)) {
    message(paste0(mc_name," to mcnode ", paste0(dim(match_prev_mcnode),collapse=", "), "\n"))
    match_prev_mcnode <- mcdata(match_prev_mcnode, type="0", nvariates=length(prenode_hg))
  }

  prev_dim <- dim(match_prev_mcnode)

  # Get current data
  data_hg <- data$hg
  data_scenario <- data$scenario_id

  message("Matching dimensions by homogeneous groups provided (node ",
          max(prenode_hg)," hg, data ", max(data_hg)," hg). From: ",
          length(unique(prenode_scenario)), " scenarios to ",
          length(unique(data_scenario))," scenarios.")

  # Handle new scenarios
  new_scenario_hg <- data_hg[!data_scenario %in% prenode_scenario]

  for(i in new_scenario_hg) {
    mc_i <- extractvar(match_prev_mcnode,i)
    match_prev_mcnode <- addvar(match_prev_mcnode,mc_i)
  }

  new_dim <- dim(match_prev_mcnode)

  message(mc_name, " prev dim: [", paste(prev_dim, collapse=", "),
          "], new dim: [", paste(new_dim, collapse=", "),"]")

  return(match_prev_mcnode)
}
