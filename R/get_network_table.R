#' Get edge table to construct a network
#'
#' @param mcmodule A mcmodule object
#'
#' @return edge_table data frame
#'
#' @examples
#' get_edge_table(mcmodule = unk_transport)
get_edge_table <- function(mcmodule) {
  node_list <- mcmodule$node_list

  edge_table <- data.frame()

  for (i in 1:length(node_list)) {
    node_to <- names(node_list)[i]
    if (node_list[[i]][["type"]] == "in_node") {
      node_from <- node_list[[i]][["inputs_col"]]
      file_from <- node_list[[i]][["input_file"]]
      edge_table_inputs <- data.frame(node_from = file_from, node_to = node_from)
    } else {
      node_from <- node_list[[i]][["inputs"]]
      edge_table_inputs <- NULL
    }
    if (!length(node_from) > 0) {
      node_from <- NA
    }
    edge_table_i <- data.frame(node_from, node_to)

    edge_table <- rbind(edge_table, edge_table_i, edge_table_inputs)
  }
  edge_table <- unique(edge_table)
  rownames(edge_table) <- NULL

  return(edge_table)
}

#' Get node table to construct a network
#'
#' @param mcmodule A mcmodule object
#'
#' @return edge_table data frame
#'
#' @examples
#' get_node_table(mcmodule = purchase)
get_node_table <- function(mcmodule, variate = 1) {
  data <- mcmodule$data
  node_list <- mcmodule$node_list

  node_table <- data.frame()


  # GET NODES INFO
  for (i in 1:length(node_list)) {
    node <- node_list[[i]]


    if (length(node[["mcnode"]]) > 0) {
      summary_value <- data.frame(summary(extractvar(node[["mcnode"]], variate))[[1]])

      if (length(summary_value$mean > 0)) {
        if (grepl("_n$|_n_|_time$", names(node_list)[i])) {
          summary_value <- summary_value %>%
            mutate(
              median = Signif(`X50.`, 2),
              up = Signif(`X2.5.`, 2),
              low = Signif(`X97.5.`, 2)
            ) %>%
            transmute(value = paste0(median, " (", up, "-", low, ")"))
        } else {
          summary_value <- summary_value %>%
            mutate(
              median = paste0(Signif(`X50.` * 100, 2), "%"),
              up = paste0(Signif(`X2.5.` * 100, 2), "%"),
              low = paste0(Signif(`X97.5.` * 100, 2), "%")
            ) %>%
            transmute(value = paste0(median, " (", up, "-", low, ")"))
        }
      } else {
        summary_value$value <- as.character(summary_value[1, ])
      }

      node_value <- summary_value$value[1]
    } else {
      node_value <- "Not Calc"
    }

    node[["mcnode"]] <- NULL

    node[["summary"]] <- NULL


    node <- lapply(node, paste, collapse = ", ")

    node_table_i <- do.call(cbind.data.frame, node)

    node_table_i$name <- names(node_list)[i]
    node_table_i$value <- node_value
    node_table <- bind_rows(node_table, node_table_i)
  }



  # GET NO-NODES INFO (files and columns)
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
        input_file = node[["input_file"]],
        value = value_col
      )

      node_table <- bind_rows(node_table, inputs_col_table)


      input_file_table <- data.frame(
        name = node[["input_file"]],
        type = "input_file"
      )
      node_table <- bind_rows(node_table, input_file_table)
    }
  }

  node_table <- node_table %>%
    relocate(name)
  rownames(node_table) <- NULL

  return(node_table)
}


#' Get visNetwork nodetable
#'
#' @param mcmodule A mcmodule object
#'
#' @return edge_table data frame
#'
#' @examples
#' get_node_table(mcmodule = purchase)
visNetwork_nodes <- function(mcmodule, variate = 1, colour = col_pal, levels = level_type) {
  mcmodule_lev <- c(0:(length(mcmodule$modules) - 1))

  names(mcmodule_lev) <- mcmodule$modules


  node_table <- get_node_table(mcmodule = mcmodule, variate = variate) %>%
    distinct(name, .keep_all = TRUE) %>%
    # filter(type=="in_node"|type=="out_node")%>%
    transmute(
      id = name,
      # type=factor(type),
      color = colour[type],
      level = ifelse(is.na(module), levels[type], levels[type] + mcmodule_lev[module]),
      module = ifelse(is.na(module), type, module),

      # value = value,
      expression = ifelse(type == "in_node", ifelse(is.na(keys), "user", ifelse(is.na(mc_func), "mcdata", mc_func)), node_expression),
      title = paste0('<p style="text-align: center;"><strong><span style="font-size: 18px;"><u>', name, '</u><br></span></strong><span style="font-size: 12px;">', module, '</span></p>
<p style="text-align: center;"><strong>', value, "<br></strong>", expression, '</p>
<table style="width: 100%; border-collapse: collapse; margin: 0px auto;">
    <tbody>
        <tr>
            <td style="width: 50%; text-align: center; background-color: rgb(239, 239, 239);"><strong>param</strong></td>
            <td style="width: 50%; text-align: center; background-color: rgb(239, 239, 239);"><strong>inputs</strong></td>
        </tr>
        <tr>
            <td style="width: 50%; text-align: center;">', gsub(",", "<br>", param), '</td>
            <td style="width: 50%; text-align: center;">', gsub(",", "<br>", inputs), "<br></td>
        </tr>
    </tbody>
</table>")
    )

  return(node_table)
}

#' Get visNetwork edges table
#'
#' @param mcmodule A mcmodule object
#'
#' @return edge_table data frame
#'
#' @examples
#' get_node_table(mcmodule = purchase)
visNetwork_edges <- function(mcmodule) {
  edge_table <- get_edge_table(mcmodule) %>%
    # filter(!is.na(node_from),node_from%in%nodes$id,node_to%in%nodes$id)%>%
    transmute(
      from = node_from,
      to = node_to,
      id = row_number()
    )
  return(edge_table)
}
