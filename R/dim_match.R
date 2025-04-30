
#' Add Group IDs to Data Frames
#'
#' @param x First dataset
#' @param by Grouping variables
#' @param y Optional second dataset
#' @return Dataframe or list of dataframes with added group IDs
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
      message("Group by: ", paste(by, sep=", "))
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
#' @param keys_names Vector of column names to extract
#'
#' @return Dataframe with scenario_id and requested key columns
#'
#' @examples
#' \dontrun{
#' keys_df <- mc_keys(mc_module, "node1", c("key1", "key2"))
#' }
#'
#' @export
mc_keys <- function(mcmodule, mc_name, keys_names=NULL) {
  # Get the node from module
  node <- mcmodule$node_list[[mc_name]]
  
  # Set keys_names from node attributes if not provided
  keys_names <- if (!is.null(keys_names)) {
    keys_names
  } else if (!is.null(node[["agg_keys"]])) {
    node[["agg_keys"]]
  } else {
    node[["keys"]]
  }
  
  # Get data from appropriate source based on node type
  data <- if (!is.null(node[["agg_keys"]])) node[["summary"]] else mcmodule$data[[node[["data_name"]]]]
  
  # Validate that all requested keys exist in the data
  missing_keys <- keys_names[!keys_names %in% names(data)]
  if (length(missing_keys) > 0) {
    stop(paste("Columns",paste(missing_keys, collapse = ", "),"not found in",mc_name,"data"))
  }
  
  # Ensure scenario_id exists, default to "0" if missing
  if (!"scenario_id" %in% names(data)) {
    data$scenario_id <- "0"
  }
  
  
  if(any(duplicated(data[data$scenario_id==0,keys_names]))){
    warning(paste("Duplicated keys in scenario 0 for", deparse(substitute(mc_name))))
  }
  
  # Return requested columns including scenario_id
  data[unique(c("scenario_id", keys_names))]
}


#' Match Monte Carlo Nodes
#'
#' Matches two mc_nodes with differing dimensions, handling null values
#'
#' @param mcmodule Monte Carlo module
#' @param mc_name_x First node name
#' @param mc_name_y Second node name
#' @param keys_names Names of key columns
#' @return List containing matched nodes and index
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
  if (nrow(keys_x) == nrow(keys_y) && all(keys_x[keys_names] == keys_y[keys_names])) {
    message(mc_name_x, " and ", mc_name_y, " already match, dim: [", 
            paste(dim(mcnode_x), collapse=", "), "]")
    
    return(list(
      mcnode_x_match = mcnode_x,
      mcnode_y_match = mcnode_y,
      index = keys_x[c(keys_names, "scenario_id")]
    ))
  }
  
  # Create cross join
  raw_cross_xy <- cross_join(
    keys_x[c("g_id","g_row", "scenario_id")],
    keys_y[c("g_id","g_row", "scenario_id")])
  
  # Find null groups
  g_null <- c(
    unique(raw_cross_xy$g_id.x[!raw_cross_xy$g_id.x %in% raw_cross_xy$g_id.y]),
    unique(raw_cross_xy$g_id.y[!raw_cross_xy$g_id.y %in% raw_cross_xy$g_id.x]))
  
  anti_filter_x <- raw_cross_xy$g_id.x %in% g_null
  anti_filter_y <- raw_cross_xy$g_id.y %in% g_null
  
  # Handle null scenarios
  anti_index_xy <- raw_cross_xy[anti_filter_x|anti_filter_y,] %>%
    filter(scenario_id.x==0|scenario_id.y==0) %>%
    filter((scenario_id.y==0 & !duplicated(paste(g_id.x, scenario_id.x))) |
             (scenario_id.x==0 & !duplicated(paste(g_id.y, scenario_id.y))) &
             !(scenario_id.y==0 & scenario_id.x==0 & 
                 ((duplicated(paste(g_id.x, scenario_id.x))) |
                 duplicated(paste(g_id.y, scenario_id.y))))) %>%
    distinct()
  
  # Check if one node has only scenario 0
  x_all_null<-all(anti_index_xy$scenario_id.x=="0")
  y_all_null<-all(anti_index_xy$scenario_id.y=="0")
  
  if(x_all_null){
    anti_index_xy <- anti_index_xy %>%
      filter(scenario_id.y==0)
  }else if(y_all_null){
    anti_index_xy <- anti_index_xy %>%
      filter(scenario_id.x==0)
  }

  index_xy <- raw_cross_xy%>%
    filter(g_id.x==g_id.y & (scenario_id.x==0|scenario_id.y==0))%>%
    rowwise()%>%
    mutate(scenario_xy=paste(min(scenario_id.x,scenario_id.y), 
                             max(scenario_id.x,scenario_id.y),
                             "-",
                             min(g_id.x,g_id.y),
                             max(g_id.x,g_id.y)))%>%
    distinct(scenario_xy, .keep_all = TRUE)
  
  index_xy <- bind_rows(index_xy,anti_index_xy) %>%
    distinct()
  
  # Match nodes
  null_x <- 0
  null_y <- 0
  
  # Process X node
  for(i in 1:nrow(index_xy)) {
    g_row_x_i <- index_xy$g_row.x[i]
    
    if(index_xy$g_id.y[i] %in% keys_x$g_id) {
      mc_i <- extractvar(mcnode_x,g_row_x_i)
    } else {
      mc_i <- extractvar(mcnode_x, 1)-extractvar(mcnode_x, 1)
      null_x <- null_x+1
    }
    
    if(!exists("mcnode_x_match")) {
      mcnode_x_match <- mc_i
    } else {
      mcnode_x_match <- addvar(mcnode_x_match,mc_i)
    }
    
  }
  
  # Process Y node
  for(i in 1:nrow(index_xy)) {
    g_row_y_i <- index_xy$g_row.y[i]
    
    if(index_xy$g_id.x[i] %in% keys_y$g_id) {
      mc_i <- extractvar(mcnode_y,g_row_y_i)
    } else {
      mc_i <- extractvar(mcnode_y, 1)-extractvar(mcnode_y, 1)
      null_y <- null_y+1
    }
    
    if(!exists("mcnode_y_match")) {
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
  
  # Create index
  index <- keys_x[ifelse(index_xy$g_id.x %in% keys_x$g_id,
                            index_xy$g_row.x,
                            index_xy$g_row.y),
                     c(keys_names)]
  
  scenario <- ifelse(
    keys_x[index_xy$g_row.x,]$scenario_id=="0",
    ifelse(keys_y[index_xy$g_row.y,]$scenario_id=="0","0",
           keys_y[index_xy$g_row.y,]$scenario_id),
    keys_x[index_xy$g_row.x,]$scenario_id)
  
  index$scenario_id <- scenario
  
  # Return results
  result <- list(mcnode_x_match, mcnode_y_match, index)
  names(result) <- c(paste0(mc_name_x,"_match"),
                     paste0(mc_name_y,"_match"),
                     "index")
  
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
