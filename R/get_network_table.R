#' Generate Edge Table for Network Construction
#'
#' Creates a data frame containing edge relationships between nodes in a Monte Carlo module network.
#' Each row represents a directed edge from one node to another.
#'
#' @param mcmodule An mcmodule object containing node relationships
#' @return A data frame with columns node_from and node_to representing network edges
#' @examples
#' edge_table <- get_edge_table(imports_mcmodule)
#' @export
get_edge_table <- function(mcmodule) {
  node_list <- mcmodule$node_list
  edge_table <- data.frame()

  if(all(is.null(unlist(sapply(node_list, "[[", "input_dataset"))))) message("input_dataset not found, using data_name")

  for (i in seq_along(node_list)) {
    node_to <- names(node_list)[i]
    if ("inputs_col"%in%names(node_list[[i]])) {
      node_from <- node_list[[i]][["inputs_col"]]
      dataset_from <- node_list[[i]][["input_dataset"]]
      data_from <- node_list[[i]][["data_name"]]

      if(!is.null(dataset_from)){
        edge_table_dataset <- data.frame(node_from = dataset_from, node_to = data_from)
        edge_table_inputs <- data.frame(node_from = data_from, node_to = node_from)
        edge_table_inputs <- rbind(edge_table_dataset, edge_table_inputs)

      }else{
        edge_table_inputs <- data.frame(node_from = data_from, node_to = node_from)
      }
    } else {
      node_from <- node_list[[i]][["inputs"]]
      edge_table_inputs <- NULL
    }

    if (!length(node_from) > 0) node_from <- NA

    edge_table_i <- data.frame(node_from, node_to)
    edge_table <- rbind(edge_table, edge_table_i, edge_table_inputs)
  }

  edge_table <- unique(edge_table)
  rownames(edge_table) <- NULL
  return(edge_table)
}

#' Generate Node Table for Network Construction
#'
#' Creates a data frame containing node information from a Monte Carlo module network.
#' Includes node attributes, values, and relationships.
#'
#' @param mcmodule An mcmodule object containing node information
#' @param variate Integer indicating which variate to extract (default: 1)
#' @return A data frame containing node information and attributes
#' @examples
#' node_table <- get_node_table(imports_mcmodule)
#' @export
get_node_table <- function(mcmodule, variate = 1) {
  data <- mcmodule$data
  node_list <- mcmodule$node_list
  node_table <- data.frame()

  # Process node information
  for (i in seq_along(node_list)) {
    node <- node_list[[i]]

    node_value <- if (length(node[["mcnode"]]) > 0) {
      summary_value <- data.frame(summary(extractvar(node[["mcnode"]], variate))[[1]])

      if (length(summary_value$mean) > 0) {
        if (grepl("_n$|_n_|_time$", names(node_list)[i])) {
          format_numeric_summary(summary_value)
        } else {
          format_percentage_summary(summary_value)
        }
      } else {
        as.character(summary_value[1, ])
      }
    } else {
      "Not Calc"
    }

    node[c("mcnode", "summary")] <- NULL
    node <- lapply(node, paste, collapse = ", ")

    node_table_i <- do.call(cbind.data.frame, node)
    node_table_i$name <- names(node_list)[i]
    node_table_i$value <- node_value
    node_table_i$inputs <- ifelse(is.na(node_table_i$inputs),node_table_i$inputs_col,node_table_i$inputs)
    node_table <- dplyr::bind_rows(node_table, node_table_i)
  }

  # Process non-node information (data-sets, data-frames and columns)
  for (i in 1:length(node_list)) {
    node <- node_list[[i]]

    if (length(node[["inputs_col"]]) > 0) {
      inputs_col <- node[["inputs_col"]]
      if (is.list(data)) {
        value_col <- c()
        for (j in 1:length(data)) {
          data_name <- names(data)[j]
          data_j <- data[[j]]
          if (all(inputs_col %in% names(data_j))) {
            value_j <- as.character(unlist(data_j[variate, inputs_col]))
            value_col <- value_j
          }
        }
      } else {
        value_col <- as.character(unlist(data[variate, inputs_col]))
      }

      if (is.null(value_col)) {
        value_col <- "Not Found"
      }

      inputs_col_table <- data.frame(
        name = node[["inputs_col"]],
        type = "inputs_col",
        inputs = paste(c(node[["data_name"]],node[["input_dataset"]]), sep = ", ", collapse = ", "),
        input_data = node[["data_name"]],
        value = value_col
      )

      node_table <- dplyr::bind_rows(node_table, inputs_col_table)

      input_data_table <- data.frame(
        name = node[["data_name"]],
        type = "input_data"
      )

      node_table <- dplyr::bind_rows(node_table, input_data_table)

      if(!is.null(node[["input_dataset"]])){
        input_dataset_table <- data.frame(
          name = node[["input_dataset"]],
          type = "input_dataset"
        )

        inputs_col_table$input_dataset<-node[["input_dataset"]]

        input_data_table <- data.frame(
          inputs = node[["input_dataset"]],
          input_dataset = node[["input_dataset"]]
        )

        node_table <- dplyr::bind_rows(node_table, input_dataset_table)
      }
    }
  }

  node_table <- dplyr::relocate(node_table,"name")
  rownames(node_table) <- NULL
  return(node_table)
}

#' Generate Network Node Table for Visualization
#'
#' Creates a formatted node table for visualization with visNetwork.
#' Includes styling and formatting for network visualization.
#'
#' @param mcmodule An mcmodule object containing the network structure
#' @param variate Integer specifying which variate to extract (default: 1)
#' @param color_pal Custom color palette for nodes (optional)
#' @param color_by Column name to determine node colors (optional)
#' @return A data frame formatted for visNetwork with columns:
#'   \itemize{
#'     \item id: Unique node identifier
#'     \item color: Node color based on type/category
#'     \item module: Module association
#'     \item expression: Node expression or type
#'     \item title: Hover text containing node details
#'   }
#' @examples
#' \dontrun{
#' nodes_df <- visNetwork_nodes(imports_mcmodule)
#' }
visNetwork_nodes <- function(mcmodule, variate = 1, color_pal = NULL, color_by = NULL) {

  nodes <- get_node_table(mcmodule = mcmodule, variate = variate)

  # Default color palette if none provided
  default_color_pal <- c(
    input_file = "#577A9E",  # Dark blue
    inputs_col = "#89A3BE",  # Medium blue
    in_node = "#BDCCDB",     # Light blue
    out_node = "#18BC9C",    # Teal
    trials_n = "#F9CF8B",    # Light orange
    subsets_n = "#DBCEB3",    # Light orange
    subsets_p = "#C1B9A5",    # Light orange
    total = "#F39C12",       # Orange
    agg_total = "#ED7427",   # Dark orange
    prev_node = "#E74C3C"    # Red
  )


  # Assing color by selected node table column
  if(is.null(color_by)) {
    # Default to coloring by "type" if no color_by specified
    color_by <- "type"
    color_levels <- levels(as.factor(nodes[[color_by]]))

    # Assign colors if palette was provided
    if(!is.null(color_pal)) {
      color_pal <- color_pal[1:length(color_levels)]
      names(color_pal) <- color_levels
    }else{
      color_pal <- default_color_pal
    }
  } else {
    # Use provided color_by column
    color_levels <- levels(as.factor(nodes[[color_by]]))

    if(is.null(color_pal)){
      # Default color palette
      color_pal <- default_color_pal[1:length(color_levels)]
      names(color_pal) <- color_levels

    }else if(is.null(names(color_pal))) {
      # Default color mapping
      color_pal <- color_pal[1:length(color_levels)]
      names(color_pal) <- color_levels
    } else {
      # Use provided color mapping
      color_pal <- color_pal[color_levels]
    }
  }

  nodes %>%
    dplyr::distinct(.data$name, .keep_all = TRUE) %>%
    dplyr::mutate(mc_func = ifelse("mc_func" %in% colnames(nodes), .data$mc_func, NA)) %>%
    dplyr::transmute(
      id = .data$name,
      color = color_pal[.data[[color_by]]],
      module = ifelse(is.na(.data$module), .data$type, .data$module),
      expression = ifelse(
        .data$type == "in_node",
        ifelse(is.na(.data$keys), "user", ifelse(is.na(.data$mc_func), "mcdata", .data$mc_func)),
        .data$node_exp
      ),
      title = generate_node_title(.data$name, .data$module, .data$value, .data$expression, .data$param, .data$inputs)
    )
}

#' Generate visNetwork Edge Table
#'
#' Creates a formatted edge table suitable for visualization with visNetwork.
#'
#' @param mcmodule An mcmodule object
#' @return A data frame containing edge information for visNetwork
#' @examples
#' \dontrun{
#' vis_edges <- visNetwork_edges(imports_mcmodule)
#' }
visNetwork_edges <- function(mcmodule) {
  get_edge_table(mcmodule) %>%
    transmute(
      from = .data$node_from,
      to = .data$node_to,
      id = row_number()
    )
}

#' Create Interactive Network Visualization
#'
#' Generates an interactive network visualization using visNetwork library. The visualization
#' includes interactive features for exploring model structure and relationships.
#'
#' @param mcmodule An mcmodule object
#' @param variate Integer specifying which variate to visualize (default: 1)
#' @param color_pal Custom color palette for nodes (optional)
#' @param color_by Column name to determine node colors (optional)
#' @return An interactive visNetwork object with features:
#'   \itemize{
#'     \item Highlighting of connected nodes
#'     \item Node selection and filtering by module
#'     \item Directional arrows showing relationships
#'     \item Hierarchical layout
#'     \item Draggable nodes
#'   }
#' @export
#' @examples
#' \dontrun{
#' network <- mc_network(imports_mcmodule)
#' }
mc_network<-function(mcmodule, variate = 1, color_pal = NULL, color_by = NULL){
  if(!all(requireNamespace("visNetwork", quietly = TRUE)&requireNamespace("igraph", quietly = TRUE))){
    stop("This function need 'visNetwork' and 'igraph' packages. Please
         install them before.")
  }

  nodes <- visNetwork_nodes(mcmodule, variate = variate, color_pal = color_pal, color_by = color_by)
  edges <- visNetwork_edges(mcmodule)

  visNetwork::visNetwork(nodes, edges, width = "100%") %>%
    visNetwork::visOptions(highlightNearest = list(enabled =TRUE, degree = 2), nodesIdSelection = TRUE,selectedBy="module")%>%
    visNetwork::visEdges(arrows = "to")%>%
    visNetwork::visIgraphLayout(layout ="layout_with_sugiyama", maxiter=500)%>%
    visNetwork::visPhysics(enabled = FALSE)%>%
    visNetwork::visInteraction(dragNodes=TRUE)
}

# Helper functions
format_numeric_summary <- function(summary_value) {
  # Extract quantiles
  median_val <- signif_round(summary_value[["X50."]], 2)
  lower_val <- signif_round(summary_value[["X2.5."]], 2)
  upper_val <- signif_round(summary_value[["X97.5."]], 2)

  # Format string
  result <- paste0(median_val, " (", lower_val, "-", upper_val, ")")

  return(result)
}

format_percentage_summary <- function(summary_value) {
  # Extract and format percentages
  median_pct <- paste0(signif_round(summary_value[["X50."]] * 100, 2), "%")
  lower_pct <- paste0(signif_round(summary_value[["X2.5."]] * 100, 2), "%")
  upper_pct <- paste0(signif_round(summary_value[["X97.5."]] * 100, 2), "%")

  # Format string
  result <- paste0(median_pct, " (", lower_pct, "-", upper_pct, ")")

  return(result)
}

generate_node_title <- function(name, module, value, expression, param, inputs) {
  paste0(
    '<p style="text-align: center;"><strong><span style="font-size: 18px;"><u>',
    name,
    '</u><br></span></strong><span style="font-size: 12px;">',
    module,
    '</span></p>
    <p style="text-align: center;"><strong>',
    ifelse(is.na(value),"",value),
    "<br></strong>",
    ifelse(is.na(expression),"",expression),
    '</p>
    <table style="width: 100%; border-collapse: collapse; margin: 0px auto;">
      <tbody>
        <tr>
          <td style="width: 50%; text-align: center; background-color: rgb(239, 239, 239);"><strong>param</strong></td>
          <td style="width: 50%; text-align: center; background-color: rgb(239, 239, 239);"><strong>inputs</strong></td>
        </tr>
        <tr>
          <td style="width: 50%; text-align: center;">',
    gsub(",", "<br>", ifelse(is.na(param),"",param)),
    '</td>
          <td style="width: 50%; text-align: center;">',
    gsub(",", "<br>", ifelse(is.na(inputs),"",inputs)),
    "<br></td>
        </tr>
      </tbody>
    </table>"
  )
}


