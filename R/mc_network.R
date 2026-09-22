#' Generate Edge Table for Network Visualisation
#'
#' Creates a data frame containing edge relationships between nodes in a
#' Monte Carlo module network. Each row represents a directed edge from one
#' node to another.
#'
#' @param mcmodule (mcmodule object). Module containing node relationships.
#' @param inputs (logical). If TRUE, include non-node inputs (datasets,
#'   dataframes, and columns). Default: FALSE.
#'
#' @return A data frame with columns `node_from` and `node_to` representing
#'   network edges.
#' @seealso [get_node_table()] and [mc_network()] for the corresponding node
#'   metadata and interactive visualisation.
#' @examples
#' edge_table <- get_edge_table(imports_mcmodule)
#' @export
get_edge_table <- function(mcmodule, inputs = FALSE) {
  node_list <- mcmodule$node_list
  edge_table <- data.frame()

  if (all(is.null(unlist(sapply(node_list, "[[", "input_dataset"))))) {
    message("input_dataset not found, using data_name")
  }

  for (i in seq_along(node_list)) {
    node_to <- names(node_list)[i]
    if (inputs & "inputs_col" %in% names(node_list[[i]])) {
      node_from <- node_list[[i]][["inputs_col"]]
      dataset_from <- node_list[[i]][["input_dataset"]]
      data_from <- node_list[[i]][["data_name"]]

      if (!is.null(dataset_from)) {
        edge_table_dataset <- data.frame(
          node_from = dataset_from,
          node_to = data_from
        )
        edge_table_inputs <- data.frame(
          node_from = data_from,
          node_to = node_from
        )
        edge_table_inputs <- rbind(edge_table_dataset, edge_table_inputs)
      } else {
        edge_table_inputs <- data.frame(
          node_from = data_from,
          node_to = node_from
        )
      }
    } else {
      node_from <- node_list[[i]][["inputs"]]
      edge_table_inputs <- NULL
    }

    if (!length(node_from) > 0) {
      next
    }

    edge_table_i <- data.frame(node_from, node_to)
    edge_table <- rbind(edge_table, edge_table_i, edge_table_inputs)
  }

  edge_table <- unique(edge_table)
  rownames(edge_table) <- NULL
  return(edge_table)
}

#' Generate Node Table for Network Visualisation
#'
#' Creates a data frame containing node information from a Monte Carlo module
#' network. Includes node attributes, values, and relationships.
#'
#' @param mcmodule (mcmodule object). Module containing node information.
#' @param variate (positive integer). Variate (row) to extract from every
#'   stochastic node and input data frame. The default uses the first variate.
#'   Use another row number when the same row has the intended meaning across
#'   the module's data frames. This argument does not filter by key values;
#'   filter the module before plotting when row positions are not comparable.
#' @param percentages (logical). If TRUE, eligible stochastic nodes whose
#'   values do not exceed 1 are displayed as percentages. Count and time nodes
#'   identified from their names are always displayed as numbers. If FALSE,
#'   all stochastic nodes are displayed as numbers. Default: TRUE.
#' @param inputs (logical). If TRUE, include non-node inputs (datasets,
#'   dataframes, and columns). Default: FALSE.
#'
#' @return A data frame containing node information and attributes.
#' @seealso [get_edge_table()] for network edges, [mc_filter()] for selecting
#'   variates by conditions, and [mc_network()] for interactive visualisation.
#' @examples
#' node_table <- get_node_table(imports_mcmodule)
#' @export
get_node_table <- function(
    mcmodule,
    variate = 1,
    inputs = FALSE,
    percentages = TRUE
) {
  if (
    length(variate) != 1 ||
    !is.numeric(variate) ||
    is.na(variate) ||
    variate < 1 ||
    variate != as.integer(variate)
  ) {
    stop("`variate` must be a single positive integer.", call. = FALSE)
  }
  if (
    length(percentages) != 1 ||
    !is.logical(percentages) ||
    is.na(percentages)
  ) {
    stop("`percentages` must be TRUE or FALSE.", call. = FALSE)
  }

  data <- mcmodule$data
  node_list <- mcmodule$node_list
  node_table <- data.frame()

  # Process node information
  for (i in seq_along(node_list)) {
    node <- node_list[[i]]

    node_value <- if (length(node[["mcnode"]]) > 0) {
      variate_value <- extractvar(
        node[["mcnode"]],
        variate
      )
      summary_value <- data.frame(summary(variate_value)[[1]])

      if (length(summary_value$mean) > 0) {
        if (should_format_percentage(
          names(node_list)[i],
          variate_value,
          percentages
        )) {
          format_percentage_summary(summary_value)
        } else {
          format_numeric_summary(summary_value)
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
    # Inputs: if 'inputs' is NA, 'inputs_col'. If 'inputs_col' is NA, NA
    # Ensure 'inputs' and 'inputs_col' exist, then prefer 'inputs' and fall back to 'inputs_col'
    if (!"inputs" %in% names(node_table_i)) {
      node_table_i$inputs <- NA
    }
    if (!"inputs_col" %in% names(node_table_i)) {
      node_table_i$inputs_col <- NA
    }

    node_table_i$inputs <- dplyr::coalesce(
      node_table_i$inputs,
      node_table_i$inputs_col
    )

    node_table <- dplyr::bind_rows(node_table, node_table_i)
  }

  # Process non-node information (data-sets, data-frames and columns)
  if (inputs) {
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
          inputs = paste(
            c(node[["data_name"]], node[["input_dataset"]]),
            sep = ", ",
            collapse = ", "
          ),
          input_data = node[["data_name"]],
          value = value_col
        )

        node_table <- dplyr::bind_rows(node_table, inputs_col_table)

        input_data_table <- data.frame(
          name = node[["data_name"]],
          type = "input_data"
        )

        node_table <- dplyr::bind_rows(node_table, input_data_table)

        if (!is.null(node[["input_dataset"]])) {
          input_dataset_table <- data.frame(
            name = node[["input_dataset"]],
            type = "input_dataset"
          )

          inputs_col_table$input_dataset <- node[["input_dataset"]]

          input_data_table <- data.frame(
            inputs = node[["input_dataset"]],
            input_dataset = node[["input_dataset"]]
          )

          node_table <- dplyr::bind_rows(node_table, input_dataset_table)
        }
      }
    }
  }

  node_table <- dplyr::relocate(node_table, "name")
  rownames(node_table) <- NULL
  return(node_table)
}

#' Generate Formatted Network Node Table for Visualisation
#'
#' Creates a formatted node table for visualisation with visNetwork.
#' Includes styling and formatting for interactive network display.
#'
#' @param mcmodule (mcmodule object). Module containing network structure.
#' @param variate (positive integer). Variate (row) to extract. See
#'   [get_node_table()] for how this applies to modules with multiple data
#'   frames. Default: 1.
#' @param percentages (logical). If TRUE, eligible nodes are displayed as
#'   percentages; if FALSE, all nodes are displayed as numbers. Default: TRUE.
#' @param color_pal (character vector, optional). Custom colour palette for nodes.
#'   Default: NULL.
#' @param color_by (character, optional). Column name to determine node colours.
#'   Default: NULL.
#' @param inputs (logical). If TRUE, include non-node inputs. Default: FALSE.
#'
#' @return A data frame formatted for visNetwork with columns: id, label, color,
#'   grouping, expression, and title (hover text).
#' @seealso [get_node_table()] and [mc_network()].
visNetwork_nodes <- function(
    mcmodule,
    variate = 1,
    color_pal = NULL,
    color_by = NULL,
    inputs = FALSE,
    percentages = TRUE
) {
  nodes <- get_node_table(
    mcmodule = mcmodule,
    variate = variate,
    percentages = percentages,
    inputs = inputs
  )

  color <- assign_color_pal(
    nodes = nodes,
    color_pal = color_pal,
    color_by = color_by
  )
  color_pal <- color[["pal"]]
  color_by <- color[["by"]]

  # Ensure all columns required by the subsequent transmute exist on `nodes`.
  required_cols <- c(
    "mc_func",
    "exp_param",
    "node_exp",
    "module",
    "exp_name",
    "type",
    "keys",
    "value",
    "inputs"
  )
  for (col in required_cols) {
    if (!col %in% colnames(nodes)) nodes[[col]] <- NA
  }

  # Ensure the dynamic color_by column exists. Prefer an existing 'color_by' column if present.
  if (!color_by %in% colnames(nodes)) {
    if ("color_by" %in% colnames(nodes)) {
      nodes[[color_by]] <- nodes[["color_by"]]
    } else {
      nodes[[color_by]] <- NA
    }
  }

  nodes <- nodes %>%
    dplyr::distinct(.data$name, .keep_all = TRUE) %>%
    dplyr::transmute(
      id = .data$name,
      color = color_pal[.data[[color_by]]],
      color_by = .data[[color_by]],
      grouping = ifelse(
        !is.na(.data$module),
        .data$module,
        ifelse(!is.na(.data$exp_name), .data$exp_name, .data$type)
      ),
      expression = ifelse(
        .data$type == "in_node",
        ifelse(
          is.na(.data$keys),
          "user",
          ifelse(is.na(.data$mc_func), "mcdata", .data$mc_func)
        ),
        .data$node_exp
      ),
      title = generate_node_title(
        .data$name,
        .data$grouping,
        .data$value,
        .data$expression,
        .data$exp_param,
        .data$inputs
      ),
      type = .data$type
    )

  if (!color_by %in% names(nodes)) {
    nodes[[color_by]] <- nodes$color_by
  }

  return(nodes)
}

#' Generate Formatted visNetwork Edge Table
#'
#' Creates a formatted edge table suitable for visualisation with visNetwork.
#'
#' @param mcmodule (mcmodule object). Module containing node relationships.
#' @param inputs (logical). If TRUE, include non-node inputs. Default: FALSE.
#'
#' @return A data frame containing edge information for visNetwork with columns:
#'   from, to, and id.
#' @seealso [get_edge_table()] and [mc_network()].
visNetwork_edges <- function(mcmodule, inputs = FALSE) {
  get_edge_table(mcmodule = mcmodule, inputs = inputs) %>%
    transmute(
      from = .data$node_from,
      to = .data$node_to,
      id = row_number()
    )
}

#' Create Interactive Network Visualisation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates an interactive network visualisation using visNetwork library.
#' The visualisation includes interactive features for exploring model structure
#' and relationships.
#'
#' By default, nodes are coloured as:
#' \itemize{
#'   \item \strong{inputs} (light blue, #B0DFF9): Input datasets, data frames, files, and columns.
#'   \item \strong{in_node} (blue, #6ABDEB): Input nodes and scalar values.
#'   \item \strong{out_node} (green, #A4CF96): Output nodes.
#'   \item \strong{filter} (light grey, #D3D3D3): Filtered nodes created with \code{mc_filter()}.
#'   \item \strong{compare} (dark grey, #707070): Comparison nodes created with \code{mc_compare()}.
#'   \item \strong{trials_info} (light orange, #FAE4CB): Trial, subset, and related information nodes used by \code{trial_totals()}, unless already classified as input or output nodes.#'   \item \strong{total} (orange, #F39200): Total nodes created with \code{at_least_one()} or \code{trial_totals()}.
#'   \item \strong{agg_total} (dark orange, #C17816): Aggregated total nodes created with \code{agg_variates()}.
#' }
#'
#' @param mcmodule (mcmodule object). Module containing network to visualise.
#' @param variate (positive integer). Variate (row) to visualise. The same row
#'   number is used across the module; this argument does not filter variates
#'   by key values. See [get_node_table()]. Default: 1.
#' @param percentages (logical). If TRUE, eligible stochastic nodes whose
#'   values do not exceed 1 are displayed as percentages. If FALSE, all
#'   stochastic nodes are displayed as numbers. Default: TRUE.
#' @param color_pal (character vector, optional). Custom colour palette for nodes.
#'   Default: NULL.
#' @param color_by (character, optional). Column name to determine node colours.
#'   Default: NULL.
#' @param legend (logical). If TRUE, show colours legend. Default: FALSE.
#' @param inputs (logical). If TRUE, show non-node inputs. Default: FALSE.
#'
#' @return An interactive visNetwork object with highlighting of connected nodes,
#'   node selection and filtering by module, directional arrows, hierarchical
#'   layout, and draggable nodes.
#' @seealso [get_node_table()] and [get_edge_table()] for the underlying tables,
#'   [mc_filter()] for selecting variates by conditions, and
#'   [visNetwork::visNetwork()] for the underlying visualisation.
#' @export
#' @examples
#' \donttest{
#' network <- mc_network(mcmodule = imports_mcmodule)
#'
#' # Select a variate by its key values rather than its original row number.
#' # The filtered node's first retained variate is displayed with variate = 1.
#' selected_module <- mc_filter(
#'   imports_mcmodule,
#'   "w_prev",
#'   pathogen == "b",
#'   origin == "south",
#'   name = "w_prev_selected"
#' )
#' selected_network <- mc_network(selected_module, variate = 1)
#' }
mc_network <- function(
    mcmodule,
    variate = 1,
    color_pal = NULL,
    color_by = NULL,
    legend = FALSE,
    inputs = FALSE,
    percentages = TRUE
) {
  if (
    !all(
      requireNamespace("visNetwork", quietly = TRUE) &
      requireNamespace("igraph", quietly = TRUE)
    )
  ) {
    stop(
      "This function needs 'visNetwork' and 'igraph' packages.
    Install them using:
         install.packages(c('visNetwork','igraph'))"
    )
  }

  nodes <- visNetwork_nodes(
    mcmodule,
    variate = variate,
    percentages = percentages,
    color_pal = color_pal,
    color_by = color_by,
    inputs = inputs
  )
  edges <- visNetwork_edges(mcmodule, inputs = inputs)

  network <- visNetwork::visNetwork(nodes, edges, width = "100%") %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 2),
      nodesIdSelection = TRUE,
      selectedBy = if (is.null(color_by)) "grouping" else color_by
    ) %>%
    visNetwork::visEdges(arrows = "to") %>%
    visNetwork::visIgraphLayout(
      layout = "layout_with_sugiyama",
      maxiter = 500
    ) %>%
    visNetwork::visPhysics(enabled = FALSE) %>%
    visNetwork::visInteraction(dragNodes = TRUE)

  if (legend) {
    color <- assign_color_pal(
      nodes = nodes,
      color_pal = color_pal,
      color_by = color_by,
      is_legend = TRUE
    )
    color_pal <- color[["pal"]]
    color_by <- color[["by"]]
    # passing custom nodes and/or edges
    lnodes <- data.frame(
      label = legend_labels(names(color_pal)),
      color = color_pal,
      shape = "dot",
      title = legend_descriptions(names(color_pal), color_by),
      font.size = 15
    )
    network <- network %>%
      visNetwork::visLegend(
        addNodes = lnodes,
        useGroups = FALSE,
        ncol = ifelse(nrow(lnodes) > 5, 2, 1),
        zoom = FALSE
      )

    return(network)
  } else {
    return(network)
  }
}

# Helper functions
should_format_percentage <- function(name, values, percentages) {
  if (!percentages || grepl("^n$|^n_|_n$|_n_|_time$", name)) {
    return(FALSE)
  }

  values <- suppressWarnings(as.numeric(values))
  !any(values > 1, na.rm = TRUE)
}

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

generate_node_title <- function(
    name,
    grouping,
    value,
    expression,
    exp_param,
    inputs
) {
  mapply(
    generate_node_title_one,
    name,
    grouping,
    value,
    expression,
    exp_param,
    inputs,
    USE.NAMES = FALSE
  )
}

generate_node_title_one <- function(
    name,
    grouping,
    value,
    expression,
    exp_param,
    inputs
) {
  parameters <- split_title_items(exp_param)
  dependencies <- setdiff(split_title_items(inputs), parameters)

  rows <- c(
    title_row("Expression / source", expression),
    title_row("Parameters", parameters),
    title_row("Other dependencies", dependencies)
  )

  paste0(
    '<p style="text-align: center;"><strong>',
    '<span style="font-size: 18px;"><u>',
    html_escape(name),
    "</u></span></strong>",
    if (!is.na(grouping) && nzchar(grouping)) {
      paste0(
        '<br><span style="font-size: 12px;">Group: ',
        html_escape(grouping),
        "</span>"
      )
    } else {
      ""
    },
    "</p>",
    if (!is.na(value) && nzchar(value)) {
      paste0(
        '<p style="text-align: center;"><strong>',
        "Value</strong><br>",
        html_escape(value),
        "</p>"
      )
    } else {
      ""
    },
    if (length(rows) > 0) {
      paste0(
        '<table style="width: 100%; border-collapse: collapse;">',
        "<tbody>",
        paste(rows, collapse = ""),
        "</tbody></table>"
      )
    } else {
      ""
    }
  )
}

split_title_items <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(character())
  }
  unique(trimws(strsplit(x, ",", fixed = TRUE)[[1]]))
}

title_row <- function(label, value) {
  if (length(value) == 0 || all(is.na(value)) || all(!nzchar(value))) {
    return(character())
  }

  paste0(
    "<tr>",
    '<td style="padding: 3px; background-color: rgb(239, 239, 239);">',
    "<strong>",
    label,
    "</strong></td>",
    '<td style="padding: 3px;">',
    paste(html_escape(value), collapse = "<br>"),
    "</td></tr>"
  )
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# Default color palette if none provided
default_color_pal <- c(
  input_dataset = "#B0DFF9",
  input_data = "#B0DFF9",
  input_file = "#B0DFF9",
  inputs_col = "#B0DFF9",
  scalar = "#6ABDEB",
  in_node = "#6ABDEB",
  out_node = "#A4CF96",
  filter = "#D3D3D3",
  compare = "#707070",
  trials_n = "#FAE4CB",
  subsets_n = "#FAE4CB",
  subsets_p = "#FAE4CB",
  total = "#F39200",
  agg_total = "#C17816"
)

default_color_legend <- c(
  inputs = "#B0DFF9",
  in_node = "#6ABDEB",
  out_node = "#A4CF96",
  filter = "#D3D3D3",
  compare = "#707070",
  trials_info = "#FAE4CB",
  total = "#F39200",
  agg_total = "#C17816"
)

default_legend_labels <- c(
  inputs = "Data inputs",
  input_dataset = "Input dataset",
  input_data = "Input data frame",
  input_file = "Input file",
  inputs_col = "Input column",
  scalar = "Scalar input",
  in_node = "Input / parameter nodes",
  out_node = "Calculated nodes",
  filter = "Filtered nodes",
  compare = "Comparison nodes",
  trials_info = "Trial / subset information",
  total = "Total nodes",
  agg_total = "Aggregated total nodes"
)

default_legend_descriptions <- c(
  inputs = "Datasets, data frames, files, and columns",
  input_dataset = "Dataset supplying model inputs",
  input_data = "Data frame supplying model inputs",
  input_file = "File supplying model inputs",
  inputs_col = "Column used to construct a model node",
  scalar = "User-supplied scalar value",
  in_node = "Model inputs, parameters, and scalar values",
  out_node = "Nodes calculated from model expressions",
  filter = "Nodes created with mc_filter()",
  compare = "Nodes created with mc_compare()",
  trials_info = "Trial, subset, and related information nodes",
  total = "Total nodes created with at_least_one()",
  agg_total = "Aggregated nodes created with agg_variates()"
)

legend_labels <- function(levels) {
  labels <- unname(default_legend_labels[levels])
  labels[is.na(labels)] <- levels[is.na(labels)]
  labels
}

legend_descriptions <- function(levels, color_by) {
  descriptions <- unname(default_legend_descriptions[levels])
  descriptions[is.na(descriptions)] <- paste0(
    color_by,
    ": ",
    levels[is.na(descriptions)]
  )
  descriptions
}

assign_color_pal <- function(nodes, color_pal, color_by, is_legend = FALSE) {
  # Assign color by selected node table column
  if (is.null(color_by)) {
    # Default to coloring by "type" if no color_by specified
    color_by <- "type"
    color_levels <- levels(as.factor(nodes[[color_by]]))

    # Assign colors if palette was not provided
    if (is.null(color_pal)) {
      color_pal <- if (is_legend) default_color_legend else default_color_pal
      color_pal <- if (is_legend) {
        color_pal[color_pal %in% nodes$color]
      } else {
        color_pal[names(color_pal) %in% color_levels]
      }
    } else {
      color_pal <- color_pal[1:length(color_levels)]
      names(color_pal) <- color_levels
    }
  } else {
    # Use provided color_by column
    color_levels <- levels(as.factor(nodes[[color_by]]))

    if (is.null(color_pal)) {
      # Default color palette
      color_pal <- default_color_legend
      color_pal <- color_pal[1:length(color_levels)]
      names(color_pal) <- color_levels
    } else if (is.null(names(color_pal))) {
      # Default color mapping
      color_pal <- color_pal[1:length(color_levels)]
      names(color_pal) <- color_levels
    } else {
      # Use provided color mapping
      color_pal <- color_pal[color_levels]
    }
  }

  return(list(pal = color_pal, by = color_by))
}
