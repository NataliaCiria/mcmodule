#' Calculate Combined Probability of Events (At least one)
#' 
#' @description Calculates the probability of at least one event occurring from a list of events,
#' assuming independence between events. This is an element-wise operation between mc_names that 
#' automatically handles row matching.
#'
#' @param mcmodule mcmodule object containing node list and input data frames
#' @param mc_names character vector of node names to combine
#' @param name optional custom name for output node
#' @param prefix optional prefix for output node name
#' @param summary logical, whether to calculate summary statistics
#'
#' @return Updated mcmodule with new combined probability node
#' @export
#'
#' @examples
#' at_least_one(mcmodule = intro,
#'              mc_names = c("no_purchase_inf_agg", "b_entry_agg"))
#'              
at_least_one <- function(mcmodule, mc_names, name = NULL, prefix = NULL, summary = TRUE) {
  
  if(!all(mc_names%in%names(mcmodule$node_list))){
    stop(paste(mc_names[!mc_names%in%names(mcmodule$node_list)],collapse = ", "), " not found in mcmodule")
  }
  
  module_name <- deparse(substitute(mcmodule))
  
  # Find largest node
  node_dim<-sapply(mc_names, function(x) dim(mcmodule$node_list[[x]][["mcnode"]])[3])
  mode_dim_max<-names(node_dim)[which.max(node_dim)]
  # Order by size
  mc_names<-mc_names[order(node_dim, decreasing=TRUE)]
  # Get data from largest mcnode
  data_name <- mcmodule$node_list[[mode_dim_max]][["data_name"]]
  data <- mcmodule$data[[data_name]]
  if(!is.null(mcmodule$node_list[[mode_dim_max]][["summary"]])){
    summary_df<-mcmodule$node_list[[mode_dim_max]][["summary"]]
    if(nrow(data)>nrow(summary_df)) data<-summary_df
  }
  
  p_all <- 0
  keys_names <- c()
  
  # Handle nodes with homogenous groups
  if (length(mcmodule$node_list[[mc_names[1]]][["hg"]]) > 0) {
    for (i in 1:length(mc_names)) {
      mc_name <- mc_names[i]
      keys_type <- ifelse("agg_keys" %in% names(mcmodule$node_list[[mc_name]]),
                          "agg_keys", "keys")
      
      keys_names <- unique(c(keys_names, 
                             mcmodule$node_list[[mc_name]][[keys_type]]))
      p_i <- mcmodule$node_list[[mc_name]][["mcnode"]]
      
      # Match dimensions if needed
      if (dim(p_i)[3] < nrow(data)) {
        p_i <- mc_match_data(mcmodule, mc_name, data)
        }
      p_all <- 1 - ((1 - p_all) * (1 - p_i))
      
      hg_index <- data$hg
    }
    
  } else {
    # Handle nodes without homogenous groups
    if (!length(mc_names) == 2) {
      stop("To aggregate mc_names without hg index provide exactly two mc_nodes")
    }
    mc_name_x <- mc_names[1]
    mc_name_y <- mc_names[2]
    
    keys_type_x <- ifelse("agg_keys" %in% names(mcmodule$node_list[[mc_name_x]]),
                          "agg_keys", "keys")
    keys_type_y <- ifelse("agg_keys" %in% names(mcmodule$node_list[[mc_name_y]]),
                          "agg_keys", "keys")
    
    keys_names_x <- unique(c(keys_names, 
                             mcmodule$node_list[[mc_name_x]][[keys_type_x]]))
    keys_names_y <- unique(c(keys_names, 
                             mcmodule$node_list[[mc_name_y]][[keys_type_y]]))
    
    keys_names <- unique(intersect(keys_names_x, keys_names_y))
    
    p_xy <- mc_match(mcmodule, mc_name_x, mc_name_y, keys_names)
    p_all <- 1 - ((1 - p_xy[[1]]) * (1 - p_xy[[2]]))
    data <- p_xy$index
    hg_index <- NULL
  }
  
  # Create new node name
  p_all_a_mc_name <- ifelse(is.null(name),
                            unique(paste0(prefix, gsub("_tr$|_pi$", "", mc_name), "_all")),
                            name)
  
  if(length(p_all_a_mc_name)>1){
    stop("If suffixes are not _tr or _pi, name for totals node must be provided")
  }
  
  # Add node to module
  mcmodule$node_list[[p_all_a_mc_name]][["mcnode"]] <- p_all
  mcmodule$node_list[[p_all_a_mc_name]][["type"]] <- "total"
  mcmodule$node_list[[p_all_a_mc_name]][["param"]] <- c(mc_names)
  mcmodule$node_list[[p_all_a_mc_name]][["inputs"]] <- c(mc_names)
  mcmodule$node_list[[p_all_a_mc_name]][["description"]] <- 
    paste("Probability at least one of", mc_names, "(assuming independence)")
  mcmodule$node_list[[p_all_a_mc_name]][["module"]] <- module_name
  mcmodule$node_list[[p_all_a_mc_name]][["keys"]] <- keys_names
  mcmodule$node_list[[p_all_a_mc_name]][["node_expression"]] <- 
    paste0("1-(", paste(paste("(1-", mc_names, ")", sep = ""), collapse = "*"), ")")
  mcmodule$node_list[[p_all_a_mc_name]][["scenario"]] <- data$scenario_id
  mcmodule$node_list[[p_all_a_mc_name]][["hg"]] <- hg_index
  mcmodule$node_list[[p_all_a_mc_name]][["data_name"]] <- data_name
  mcmodule$node_list[[p_all_a_mc_name]][["prefix"]] <- prefix
  
  if (summary) {
    mcmodule$node_list[[p_all_a_mc_name]][["summary"]] <- 
      mc_summary(data = data,
                 mcnode = p_all,
                 mc_name = p_all_a_mc_name,
                 keys_names = keys_names)
  }
  
  return(mcmodule)
}

#' Calculate Multi-Level Total Probabilities and Expected Counts
#'
#' @description
#' Calculates hierarchical probabilities and expected counts across different levels
#' (for example: trial, subset, set) using trial probabilities and population structure.
#' Handles nested sampling with conditional probabilities.
#'
#' @param mcmodule An mcmodule object with input data and node structure
#' @param mc_names Vector of node names to process
#' @param trials_n Trial count column name (default: "animals_n")
#' @param subsets_n Subset count column name (default: "farms_n")
#' @param subsets_p Subset prevalence column name (default: NULL)
#' @param name Custom name for output nodes (optional)
#' @param prefix Prefix for output node names (optional)
#' @param all_mc_names If TRUE, processes all nodes (default: TRUE)
#' @param summary If TRUE, includes summary statistics (default: calc_summary)
#' @param agg_keys Vector of column names to aggregate by (optional)
#' @param suffix Suffix for aggregated node names (default: "agg")
#'
#' @return
#' Updated mcmodule object with:
#' - Combined probabilities across nodes
#' - Trial-level probabilities and counts
#' - Subset-level probabilities and counts 
#' - Set-level probabilities and counts
#'
#' @examples
#' result <- get_totals2(
#'   mcmodule = purchase_origin,
#'   mc_names = c("a_inf", "a_inf_pi", "a_inf_tr")
#' )
#'
#' @export
get_totals <- function(mcmodule, mc_names, trials_n = "animals_n", 
                        subsets_n = "farms_n", subsets_p = NULL,
                        name = NULL, suffix=NULL, prefix = NULL,
                        all_mc_names = TRUE, agg_keys=NULL,
                       keep_variates = FALSE,
                       summary = TRUE) {
  
  data_name <- mcmodule$node_list[[mc_names[1]]][["data_name"]]
  data <- mcmodule$data[[data_name]]
  module_name <- deparse(substitute(mcmodule))
  
  # Function to handle individual mcnode creation and processing
  process_mcnode <- function(mc_name, node_type, mcmodule, data, module_name, admin_wif, agg_keys, suffix, agg_avg) {
    if (mc_name %in% names(mcmodule$node_list)) {
      mc_node <- mcmodule$node_list[[mc_name]][["mcnode"]]
      new <- FALSE
    } else if (mc_name %in% names(data)) {
      mc_node <- mcdata(data[[mc_name]], type = "0", nvariates = nrow(data))
      new <- TRUE
    } else {
      create_mc_nodes(mcmodule$data,
                      mctable = mcnode_admin[mcnode_admin$mcnode %in% mc_name,])
      mc_node <- get(mc_name)
      new <- TRUE
    }
    
    # Add to node list
    mcmodule$node_list[[mc_name]][["type"]] <- node_type
    mcmodule$node_list[[mc_name]][["module"]] <- module_name
    
    if (admin_wif) {
      if (new) {
        mcmodule$node_list[[mc_name]][["scenario"]] <- data$scenario_id
        mcmodule$node_list[[mc_name]][["hg"]] <- data$hg
      }
      if (dim(mc_node)[3] < nrow(data)) {
        mc_node <- mc_match_data(mcmodule, mc_name, data)
      }
    }
    
    mcmodule$node_list[[mc_name]][["mcnode"]] <- mc_node
    
    if(!is.null(agg_keys)){
      mcmodule$node_list[[mc_name]][["data_name"]] <- data_name
      # Aggregate node if agg_keys provided
      mcmodule<-get_agg_totals(mcmodule, mc_name, keys_names = agg_keys, suffix, agg_avg=agg_avg, keep_variates=keep_variates)
      # Change mcnode name to agg version name
      mc_name_name <- deparse(substitute(mc_name))
      assign(mc_name_name,paste0(mc_name,"_",suffix), envir = parent.frame())
      #Add agg_keys to metadata
      mcmodule$node_list[[mc_name]][["agg_keys"]]<-agg_keys
      # Reassign mcmodule name (defaults to "mcmodule")
      mcmodule$node_list[[mc_name]][["module"]]<-module_name
    }
    return(mcmodule)
  }
  
  # Process all nodes
  
  mcmodule <- process_mcnode(trials_n, "trials_n", mcmodule, data, module_name, admin_wif, agg_keys, suffix, agg_avg=FALSE)
  trials_n_mc<-mcmodule$node_list[[trials_n]][["mcnode"]]
  
  
  # If subsets_n is NULL, defaults to 1
  if(is.null(subsets_n)){
    subsets_n_mc<-mcnode_na_rm(trials_n_mc/trials_n_mc,1)
    subsets_n<-"1"
  }else{
    mcmodule <- process_mcnode(subsets_n, "subsets_n", mcmodule, data, module_name, admin_wif, agg_keys, suffix, agg_avg=TRUE)
    subsets_n_mc<-mcmodule$node_list[[subsets_n]][["mcnode"]]
  }
  
  # If subsets_p is NULL, no multilevel probability, defaults to 1
  if(is.null(subsets_p)){
    multilevel<-FALSE
    subsets_p_mc<-mcnode_na_rm(trials_n_mc/trials_n_mc,1)
    subsets_p<-"1"
  }else{
    multilevel<-TRUE
    mcmodule <- process_mcnode(subsets_p, "subsets_p", mcmodule, data, module_name, admin_wif, agg_keys, suffix, agg_avg=TRUE)
    subsets_p_mc<-mcmodule$node_list[[subsets_p]][["mcnode"]]
  }
  
  
  prefix <- ifelse(is.null(prefix), "", paste0(prefix, "_"))
  
  # Calculate combined probability for all nodes if requested
  # (if more than one node is provided)
  if (all_mc_names&length(mc_names)>1) {
    mcmodule <- at_least_one(mcmodule, mc_names, name, prefix,
                             summary, module_name)
    
    # Generate new MC node name if name is not provided
    p_all_a_mc_name <- ifelse(is.null(name),
                              unique(paste0(prefix, gsub("_tr$|_pi$", "", mc_names), "_all")),
                              name)
    
    if (length(p_all_a_mc_name) > 1) {
      stop("Name for totals node required when suffixes are not _tr or _pi")
    }
    
    mc_names <- c(mc_names, p_all_a_mc_name)
  }
  
  # Helper function to add metadata to nodes
  add_mc_metadata <- function(node_list, name, value, params, description, expression, type = "total", keys_names, agg_keys, total_type) {
    node_list[[name]] <- list(
      mcnode = value,
      param = params,
      inputs = params,
      description = description,
      node_expression = expression,
      type = type,
      module = module_name,
      keys = keys_names,
      scenario = data$scenario_id,
      hg = data$hg,
      data_name = data_name,
      prefix = prefix,
      total_type = total_type
    )
    
    if (!is.null(agg_keys)) {
      node_list[[name]]$agg_keys <- agg_keys
    }
    
    node_list
  }
  
  # Configuration for name replacements
  name_replacements <- list(
    trial = list(from = "^a", to = "t", level = "_t_"),
    subset = list(from = "^a", to = "f", level = "_f_"),
    set = list(from = "^a", to = "b", level = "_b_")
  )
  
  
  # Configuration for calculations
  calculations <- list(
    trial = list(
      prob = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc) p_a * subsets_p_mc,
        description = "Probability of one %s trial",
        suffix = "",
        expression = function(mc_name, subsets_p) paste0(subsets_p,"*", mc_name)
      ),
      num = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc) mcnode_na_rm(p_a/p_a,1),
        description = "One %s trials",
        suffix = "_n", 
        expression = function(mc_name) paste0("mcnode_na_rm(",mc_name,"/",mc_name,", 1)")
      )
    ),
    subset = list(
      prob = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc) 1-(1- subsets_p_mc*(1 - (1 - p_a)^trials_n_mc)),
        description = "Probability of at least one %s in a subset",
        suffix = "",
        expression = function(mc_name, trials_n, subsets_p) paste0("1-(1-",subsets_p,"*(1-(1-", mc_name, ")^", trials_n,"))")
      ),
      num = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc)
          p_a * trials_n_mc * subsets_p_mc,
        description = "Expected number of %s in a subset",
        suffix = "_n",
        expression = function(mc_name, trials_n, subsets_p)
          paste0(mc_name, "*", trials_n, "*", subsets_p)
      )
    ),
    set = list(
      prob = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc)
          1-(1-subsets_p_mc*(1-(1-p_a)^trials_n_mc))^subsets_n_mc,
        description = "Probability of at least one %s in a set",
        suffix = "",
        expression = function(mc_name, trials_n, subsets_n, subsets_p)
          paste0("1-(1-", subsets_p, "*(1-(1-", mc_name, ")^", trials_n, "))^", subsets_n)
      ),
      num = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc)
          p_a * trials_n_mc * subsets_p_mc * subsets_n_mc,
        description = "Expected number of %s in a set",
        suffix = "_n",
        expression = function(mc_name, trials_n, subsets_n, subsets_p)
          paste0(mc_name, "*", trials_n, "*", subsets_p, "*", subsets_n)
      )
    )
  )
  
  # Process each node
  for (mc_name in mc_names) {
    if(!is.null(agg_keys)){
      # Aggregate node if agg_keys provided
      mcmodule<-get_agg_totals(mcmodule, mc_name, keys_names = agg_keys, suffix, keep_variates = keep_variates)
      # Change mcnode name to agg version name
      assign("mc_name",paste0(mc_name,"_",suffix))
      keys_names <- agg_keys
      # Reassign mcmodule name (defaults to "mcmodule")
      mcmodule$node_list[[mc_name]][["module"]]<-module_name
      #Add agg_keys to metadata
      mcmodule$node_list[[mc_name]][["agg_keys"]]<-agg_keys
    }else{
      keys_names <- mcmodule$node_list[[mc_name]][["keys"]]
    }
    
    clean_mc_name <- gsub(prefix, "", mc_name)
    p_a <- mcmodule$node_list[[mc_name]][["mcnode"]]
    
    
    if (dim(p_a)[3] < nrow(data)&is.null(agg_keys)) {
      p_a <- mc_match_data(mcmodule, mc_name, data)
    }
    
    # Process trial, subset and set levels
    for (level in c("trial", "subset", "set")) {
      base_name <- gsub(
        name_replacements[[level]]$from,
        name_replacements[[level]]$to,
        clean_mc_name
      )
      
      # Check if "_a_" is in the base_name, if not, add the level suffix
      if (!grepl("^a_|_a_", mc_name)) {
        base_name <- paste0(base_name, "_", level)
      } else {
        base_name <- gsub("_a_", name_replacements[[level]]$level, base_name)
      }
      
      # Process probability and number calculations
      for (calc_type in c("prob", "num")) {
        calc <- calculations[[level]][[calc_type]]
        new_mc_name <- paste0(prefix, base_name, calc$suffix)

        # Calculate value based on level
        value <- calc$formula(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc)
        total_type<-paste(ifelse(multilevel,"multilevel","single level"),level,calc_type)
        
        # Create node and add metadata
        mcmodule$node_list <- add_mc_metadata(
          node_list = mcmodule$node_list,
          name = new_mc_name,
          value = value,
          params = if (level == "trial") {
            if (calc_type == "prob") c(mc_name, subsets_p) else c()
          } else if (level == "subset") {
            c(mc_name, trials_n, subsets_p)
          } else {
            c(mc_name, trials_n, subsets_n, subsets_p)
          },
          description = sprintf(calc$description, mc_name),
          expression = if (level == "trial") {
            if (calc_type == "prob") calc$expression(mc_name, subsets_p)
            else calc$expression(mc_name)
          } else if (level == "subset") {
            calc$expression(mc_name, trials_n, subsets_p)
          } else {
            calc$expression(mc_name, trials_n, subsets_n, subsets_p)
          },
          keys_names = keys_names,
          agg_keys = agg_keys,
          total_type = total_type
        )
        
        # Add summary if requested
        if (summary) {
          if(!is.null(agg_keys)){
            mcmodule$node_list[[new_mc_name]][["summary"]] <- mc_summary(
              data = mcmodule$node_list[[mc_name]][["summary"]],
              mcnode = mcmodule$node_list[[new_mc_name]][["mcnode"]],
              mc_name = new_mc_name,
              keys_names = agg_keys
            )
          }else{
            mcmodule$node_list[[new_mc_name]][["summary"]] <- mc_summary(
              data = data,
              mcnode = mcmodule$node_list[[new_mc_name]][["mcnode"]],
              mc_name = new_mc_name,
              keys_names = keys_names)
          }
        }
      }
    }
  }
  
  mcmodule$modules <- unique(c(mcmodule$modules, module_name))
  return(mcmodule)
}


#' Calculate Aggregated Totals Across Groups
#'
#' Aggregates node values across specified grouping variables
#'
#' @param mcmodule mcmodule object containing the data
#' @param mcnode name of node to aggregate
#' @param keys_names grouping variables for aggregation
#' @param suffix suffix for output node name
#' @param data_name name/index of data to use
#' @param summary logical, whether to calculate summaries
#' @param keep_variates logical, whether to keep all variates
#' @param agg_avg Logical; if TRUE, calculates average values instead of totals or combined probabilities (default: FALSE)
#'
#' @return Updated mcmodule with new aggregated nodes
#' @export
#'
#' @examples
#' get_agg_totals(mcmodule = purchase_origin,
#'                mcnode = "a_inf_all")
get_agg_totals <- function(mcmodule, mc_name, 
                           keys_names = c("pathogen", "farm_id", "scenario_id"),
                           suffix = "agg",
                           summary = TRUE,
                           keep_variates = FALSE,
                           agg_avg=FALSE) {
  
  module_name <- deparse(substitute(mcmodule))
  mcnode <- mcmodule$node_list[[mc_name]][["mcnode"]]
  
  # Get appropriate data
  data_name <- mcmodule$node_list[[mc_name]][["data_name"]]
  data <- mcmodule$data[[data_name]]
  
  if(!is.null(mcmodule$node_list[[mc_name]][["summary"]])){
    summary_df<-mcmodule$node_list[[mc_name]][["summary"]]
    if(nrow(data)>nrow(summary_df)) data<-summary_df
  }

  # Match dimensions if needed
  if (dim(mcnode)[3] < nrow(data)) {
    mcnode <- mc_match_data(mcmodule, mc_name, data)
  }
  
  agg_total_mc_name <- paste0(mc_name, "_", suffix)
  
  # Extract variates
  variates_list <- list()
  inv_variates_list <- list()
  for (i in 1:dim(mcnode)[3]) {
    variates_list[[i]] <- extractvar(mcnode, i)
    inv_variates_list[[i]] <- 1 - extractvar(mcnode, i)
  }
  
  # Create grouping index
  key_col <- data %>%
    select(all_of(keys_names)) %>%
    unite(everything(), col = "key", sep = ", ", remove = FALSE)
  
  key_levels <- unique(key_col$key)
  
  # Process each group
  for (i in 1:length(key_levels)) {
    index <- key_col$key %in% key_levels[i]
    
    if(agg_avg){
      # Calculate average value
      total_lev <- Reduce("+", variates_list[index])/sum(index)
      mcmodule$node_list[[agg_total_mc_name]][["description"]] <- 
        paste("Average value:", 
              paste(keys_names, collapse = ", "))
      mcmodule$node_list[[agg_total_mc_name]][["node_expression"]] <- 
        paste0("Average ", mc_name," by: ",
               paste(keys_names, collapse = ", "))
    }else if (grepl("_n$", mc_name)) {
      # Sum for counts
      total_lev <- Reduce("+", variates_list[index])
      mcmodule$node_list[[agg_total_mc_name]][["description"]] <- 
        paste("Number expected events by:", 
              paste(keys_names, collapse = ", "))
      mcmodule$node_list[[agg_total_mc_name]][["node_expression"]] <- 
        paste0(mc_name, "_1+", mc_name, "_2+... by:",
               paste(keys_names, collapse = ", "))
    } else {
      # Combine probabilities
      total_lev <- 1 - Reduce("*", inv_variates_list[index])
      mcmodule$node_list[[agg_total_mc_name]][["description"]] <- 
        paste("Probability at least one of the events happening by:",
              paste(keys_names, collapse = ", "))
      mcmodule$node_list[[agg_total_mc_name]][["node_expression"]] <- 
        paste0("1-((1-", mc_name, "_1)*(1-", mc_name, "_2)...) by:",
               paste(keys_names, collapse = ", "))
    }
    
    # Aggregate results
    if (keep_variates) {
      # One row per original variate
      agg_index <- mcdata(index, type = "0", nvariates = length(index))
      
      if (exists("total_agg")) {
        total_agg <- total_agg + agg_index * total_lev
      } else {
        total_agg <- agg_index * total_lev
      }
      
      new_keys_names <- mcmodule$node_list[[mc_name]][["keys"]]
      key_data <- data
    } else {
      # One row per result
      if (exists("total_agg")) {
        total_agg <- addvar(total_agg, total_lev)
      } else {
        total_agg <- total_lev
      }
      new_keys_names <- keys_names
      key_data <- unique(key_col)
    }
  }
  
  # Add aggregated node to module
  mcmodule$node_list[[agg_total_mc_name]][["mcnode"]] <- total_agg
  mcmodule$node_list[[agg_total_mc_name]][["type"]] <- "agg_total"
  mcmodule$node_list[[agg_total_mc_name]][["module"]] <- module_name
  mcmodule$node_list[[agg_total_mc_name]][["agg_data"]] <- key_levels
  mcmodule$node_list[[agg_total_mc_name]][["agg_keys"]] <- new_keys_names
  mcmodule$node_list[[agg_total_mc_name]][["keys"]] <- 
  mcmodule$node_list[[mc_name]][["keys"]]
  mcmodule$node_list[[agg_total_mc_name]][["inputs"]] <- mc_name
  mcmodule$node_list[[agg_total_mc_name]][["data_name"]] <- data_name
  
  if (summary) {
    mcmodule$node_list[[agg_total_mc_name]][["summary"]] <- 
      mc_summary(data = key_data,
                 mcnode = total_agg,
                 mc_name = agg_total_mc_name,
                 keys_names = new_keys_names)
  }
  
  mcmodule$modules <- unique(c(mcmodule$modules, module_name))
  return(mcmodule)
}
