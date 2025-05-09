#' Generate Edge Table for Network Construction
#'
#' Creates a data frame containing edge relationships between nodes in a Monte Carlo module network.
#' Each row represents a directed edge from one node to another.
#'
#' @param mcmodule An mcmodule object containing node relationships
#' @return A data frame with columns node_from and node_to representing network edges
#' @export
#' @examples
#' \dontrun{
#' edge_table <- get_edge_table(my_mcmodule)
#' }
get_edge_table <- function(mcmodule) {
  node_list <- mcmodule$node_list
  edge_table <- data.frame()

  if(all(is.null(unlist(sapply(node_list, "[[", "input_dataset"))))) message("input_dataset not found, using data_name")

  for (i in seq_along(node_list)) {
    node_to <- names(node_list)[i]
    if (node_list[[i]][["type"]] == "in_node") {
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
#' @export
#' @examples
#' \dontrun{
#' node_table <- get_node_table(my_mcmodule)
#' }
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
            value_col <- paste(value_col, paste0(data_name, "=", value_j))
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
        input_data = node[["data_name"]],
        input_dataset = node[["input_dataset"]],
        value = value_col
      )
      node_table <- dplyr::bind_rows(node_table, inputs_col_table)

      input_data_table <- data.frame(
        name = node[["data_name"]],
        input_dataset = node[["input_dataset"]],
        type = "input_data"
      )

      node_table <- dplyr::bind_rows(node_table, input_data_table)

      if(!is.null(node[["input_dataset"]])){
        input_dataset_table <- data.frame(
          name = node[["input_dataset"]],
          type = "input_dataset"
        )
        node_table <- dplyr::bind_rows(node_table, input_dataset_table)
      }

    }
  }

  node_table <- node_table %>% relocate(name)
  rownames(node_table) <- NULL
  return(node_table)
}

#' Generate visNetwork Node Table
#'
#' Creates a formatted node table suitable for visualization with visNetwork.
#' Includes styling and formatting for network visualization.
#'
#' @param mcmodule An mcmodule object
#' @param variate Integer indicating which variate to extract (default: 1)
#' @param colour Color palette for nodes
#' @param levels Level types for node positioning
#' @return A data frame formatted for visNetwork visualization
#' @export
#' @examples
#' \dontrun{
#' vis_nodes <- visNetwork_nodes(my_mcmodule)
#' }
visNetwork_nodes <- function(mcmodule, variate = 1, colour = col_pal, levels = level_type) {
  mcmodule_lev <- seq(0, length(mcmodule$modules) - 1)
  names(mcmodule_lev) <- mcmodule$modules

  get_node_table(mcmodule = mcmodule, variate = variate) %>%
    distinct(name, .keep_all = TRUE) %>%
    transmute(
      id = name,
      color = colour[type],
      level = ifelse(is.na(module), levels[type], levels[type] + mcmodule_lev[module]),
      module = ifelse(is.na(module), type, module),
      expression = ifelse(
        type == "in_node",
        ifelse(is.na(keys), "user", ifelse(is.na(mc_func), "mcdata", mc_func)),
        node_expression
      ),
      title = generate_node_title(name, module, value, expression, param, inputs)
    )
}

#' Generate visNetwork Edge Table
#'
#' Creates a formatted edge table suitable for visualization with visNetwork.
#'
#' @param mcmodule An mcmodule object
#' @return A data frame containing edge information for visNetwork
#' @export
#' @examples
#' \dontrun{
#' vis_edges <- visNetwork_edges(my_mcmodule)
#' }
visNetwork_edges <- function(mcmodule) {
  get_edge_table(mcmodule) %>%
    transmute(
      from = node_from,
      to = node_to,
      id = row_number()
    )
}

# Helper functions
format_numeric_summary <- function(summary_value) {
  summary_value %>%
    mutate(
      median = signif_round(`X50.`, 2),
      up = signif_round(`X2.5.`, 2),
      low = signif_round(`X97.5.`, 2)
    ) %>%
    transmute(value = paste0(median, " (", up, "-", low, ")")) %>%
    pull(value)
}

format_percentage_summary <- function(summary_value) {
  summary_value %>%
    mutate(
      median = paste0(signif_round(`X50.` * 100, 2), "%"),
      up = paste0(signif_round(`X2.5.` * 100, 2), "%"),
      low = paste0(signif_round(`X97.5.` * 100, 2), "%")
    ) %>%
    transmute(value = paste0(median, " (", up, "-", low, ")")) %>%
    pull(value)
}

generate_node_title <- function(name, module, value, expression, param, inputs) {
  paste0(
    '<p style="text-align: center;"><strong><span style="font-size: 18px;"><u>',
    name,
    '</u><br></span></strong><span style="font-size: 12px;">',
    module,
    '</span></p>
    <p style="text-align: center;"><strong>',
    value,
    "<br></strong>",
    expression,
    '</p>
    <table style="width: 100%; border-collapse: collapse; margin: 0px auto;">
      <tbody>
        <tr>
          <td style="width: 50%; text-align: center; background-color: rgb(239, 239, 239);"><strong>param</strong></td>
          <td style="width: 50%; text-align: center; background-color: rgb(239, 239, 239);"><strong>inputs</strong></td>
        </tr>
        <tr>
          <td style="width: 50%; text-align: center;">',
    gsub(",", "<br>", param),
    '</td>
          <td style="width: 50%; text-align: center;">',
    gsub(",", "<br>", inputs),
    "<br></td>
        </tr>
      </tbody>
    </table>"
  )
}
