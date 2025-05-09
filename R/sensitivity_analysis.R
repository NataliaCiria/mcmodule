#' Monte Carlo Module Index
#'
#' @param mcmodule A Monte Carlo module object
#' @return A list containing expression data, data keys, and global keys
#' @export
mcmodule_index <- function(mcmodule) {
  # Extract module expressions and metadata
  expression <- unlist(lapply(names(mcmodule$node_list), \(x) mcmodule$node_list[[x]][["module"]]))
  data_name <- unlist(lapply(names(mcmodule$node_list), \(x) mcmodule$node_list[[x]][["data_name"]] %||% NA))
  type <- unlist(lapply(names(mcmodule$node_list), \(x) mcmodule$node_list[[x]][["type"]] %||% NA))

  # Extract keys
  agg_keys_list <- lapply(names(mcmodule$node_list), \(x) mcmodule$node_list[[x]][["agg_keys"]] %||% NA)
  keys_list <- lapply(names(mcmodule$node_list), \(x) mcmodule$node_list[[x]][["keys"]] %||% NA)

  # Process keys for each unique expression
  keys <- data.frame()
  for (i in unique(expression)) {
    keys_list_i <- unique(unlist(keys_list[expression %in% i]))
    agg_keys_list_i <- unique(unlist(agg_keys_list[expression %in% i]))

    keys_list_i <- if (length(keys_list_i[!is.na(keys_list_i)]) < 1) NA else paste(keys_list_i[!is.na(keys_list_i)], collapse = ", ")
    agg_keys_list_i <- if (length(agg_keys_list_i[!is.na(agg_keys_list_i)]) < 1) NA else paste(agg_keys_list_i[!is.na(agg_keys_list_i)], collapse = ", ")

    keys <- bind_rows(keys, data.frame(keys = keys_list_i, agg_keys = agg_keys_list_i, expression = i))
  }

  # Process expression data
  expression_data <- data.frame(expression, data_name, type) %>%
    filter(!is.na(expression) & !is.na(data_name) & !grepl("total", type)) %>%
    select(expression, data_name) %>%
    distinct() %>%
    filter(!duplicated(expression))

  # Process global keys
  global_keys <- unique(unlist(keys_list))
  global_keys <- global_keys[!is.na(global_keys)]

  # Process data keys
  data_keys <- data.frame()
  for (i in unique(data_name)[unique(data_name) %in% names(mcmodule$data)]) {
    data_i <- mcmodule$data[[i]][names(mcmodule$data[[i]]) %in% global_keys]
    data_i$variate <- seq_len(nrow(data_i))
    data_i$data_name <- i
    data_keys <- bind_rows(data_keys, data_i)
  }

  list(expression_data = expression_data, data_keys = data_keys, global_keys = global_keys)
}

#' Calculate Relative Change in Monte Carlo Module
#'
#' @param mcmodule_def Default mcmoduleduction module
#' @param mcmodule_alt Alternative mcmoduleduction module
#' @param mc_names Optional names of Monte Carlo nodes
#' @param mcnode_admin Monte Carlo node administration object
#' @return Modified mcmoduleduction module with relative changes
#' @export
mcmodule_rel_change <- function(mcmodule_def, mcmodule_alt, mc_names = NULL, mcnode_admin = mcnode_admin) {
  mcmodule_rel <- mcmodule_def
  mc_names <- mc_names %||% names(mcmodule_rel$node_list)
  mcmodule_rel$node_list <- mcmodule_rel$node_list[mc_names]

  pb <- txtProgressBar(min = 0, max = length(mc_names), initial = 0, style = 3)

  for (i in seq_along(mc_names)) {
    mcnode_def <- mcmodule_def$node_list[[mc_names[i]]][["mcnode"]]
    mcnode_alt <- mcmodule_alt$node_list[[mc_names[i]]][["mcnode"]]

    if (!typemcnode(mcnode_def) == "0") {
      mcnode_cor <- cornode(mcnode_def, mcnode_alt, target = 0.9999999, outrank = FALSE, result = FALSE)
      mcnode_def <- mcnode_cor[[1]]
      mcnode_alt <- mcnode_cor[[2]]
    }

    # Calculate relative change
    mcnode_rel <- mcnode_na_rm((mcnode_alt - mcnode_def) / mcnode_def)
    mcmodule_rel$node_list[[mc_names[i]]][["mcnode"]] <- mcnode_rel

    # Update summary if it exists
    if (!is.null(mcmodule_rel$node_list[[mc_names[i]]][["summary"]])) {
      summary_rel_l <- summary(mcnode_rel)[[1]]
      if (!is.list(summary_rel_l)) summary_rel_l <- list(summary_rel_l)

      summary_names <- colnames(summary_rel_l[[1]])
      summary_rel <- data.frame(matrix(unlist(summary_rel_l), nrow = length(summary_rel_l), byrow = TRUE))
      names(summary_rel) <- summary_names

      summary_def <- mcmodule_rel$node_list[[mc_names[i]]][["summary"]]
      summary_def[summary_names] <- NULL
      mcmodule_rel$node_list[[mc_names[i]]][["summary"]] <- cbind(summary_def, summary_rel)
    }

    setTxtProgressBar(pb, i)
  }

  close(pb)
  mcmodule_rel
}

#' Calculate Spearman Correlation for Monte Carlo Module
#'
#' @param mcmodule Monte Carlo module object
#' @param agg_output Name of aggregated output, defaults to "total_agg"
#' @return Data frame with Spearman correlation results
#' @export
mcmodule_spearman <- function(mcmodule, agg_output = "total_agg") {
  exp_names <- names(mcmodule$mc_list)
  spearman_rho_exp <- spearman_rho_agg <- data.frame()

  mcmodule_index <- mcmodule_index(mcmodule)

  # Get aggregated output
  mc_agg <- mcmodule$node_list[[agg_output]][["mcnode"]]
  summary_agg <- mcmodule$node_list[[agg_output]][["summary"]]
  summary_agg$variate <- seq_len(nrow(summary_agg))

  pb <- txtProgressBar(min = 0, max = length(exp_names), initial = 0, style = 3)

  # Process each expression
  for (h in seq_along(mcmodule$mc_list)) {
    expression_h <- names(mcmodule$mc_list)[h]
    variates <- mcmodule$mc_list[[h]]

    data_name_h <- mcmodule_index$expression_data$data_name[mcmodule_index$expression_data$expression %in% expression_h]
    data_h <- mcmodule$data[[data_name_h]]
    keys_h <- names(data_h)[names(data_h) %in% mcmodule_index$global_keys & names(data_h) %in% names(summary_agg)]
    data_h <- data_h[keys_h]

    # Process each variate
    for (i in seq_along(variates)) {
      variate_i <- variates[[i]]

      suppressMessages({
        data_i <- data_h[i, ] %>% left_join(summary_agg[c("variate", keys_h)])
      })

      # Process input nodes
      input_nodes_exp <- variate_i[names(variate_i) %in% as.character(mcnode_admin$mcnode)]
      input_exp <- NULL

      for (j in seq_along(input_nodes_exp)) {
        input_node_exp_j <- input_nodes_exp[[j]]
        new_input_exp <- mc(input_node_exp_j, name = names(input_nodes_exp[j]))
        input_exp <- if (is.null(input_exp)) new_input_exp else mc(input_exp, new_input_exp)
      }

      # Process outputs
      if (!is.null(variate_i$output)) {
        output_node_exp <- variate_i$output
        mc_exp_i <- mc(input_exp, mc(output_node_exp, name = "output"))

        suppressWarnings({
          spearman_rho_exp_i <- as.data.frame(tornado(mc_exp_i)$value$output)
        })
        spearman_rho_exp_i$variate <- i
        spearman_rho_exp_i$expression <- exp_names[[h]]
        spearman_rho_exp_i$nest <- names(exp_names[h])
        spearman_rho_exp <- bind_rows(spearman_rho_exp, spearman_rho_exp_i)
      } else {
        mc_exp_i <- input_exp
      }

      # Process aggregated outputs
      output_node_agg <- extractvar(mc_agg, data_i$variate)
      mc_agg_i <- if (!is.null(variate_i$output)) {
        mc(input_exp, mc(output_node_exp, name = "output"), mc(output_node_agg, name = "agg_output"))
      } else {
        mc(input_exp, mc(output_node_agg, name = "agg_output"))
      }

      if (!all(dimmc(input_exp) == 1)) {
        suppressWarnings({
          spearman_rho_agg_i <- as.data.frame(tornado(mc_agg_i)$value$agg_output)
        })
        spearman_rho_agg_i$variate <- i
        spearman_rho_agg_i$expression <- exp_names[[h]]
        spearman_rho_agg_i$nest <- names(exp_names[h])
        spearman_rho_agg <- bind_rows(spearman_rho_agg, spearman_rho_agg_i)
      }
    }
    setTxtProgressBar(pb, h)
  }

  close(pb)

  # Prepare final data
  spearman_data_agg <- spearman_rho_agg %>%
    pivot_longer(-c("variate", "expression"), names_to = "node", values_to = "value_agg")

  spearman_data_exp <- spearman_rho_exp %>%
    pivot_longer(-c("variate", "expression"), names_to = "node", values_to = "value_exp")

  spearman_data <- spearman_data_agg %>%
    left_join(spearman_data_exp) %>%
    filter(!is.na(value_agg)) %>%
    left_join(mcmodule_index$expression_data) %>%
    left_join(mcmodule_index$data_keys)

  spearman_data
}
#' Monte Carlo Simulation Convergence Analysis
#'
#' @description
#' Analyzes convergence in Monte Carlo simulations by computing statistical measures
#' across iterations. Calculates both standardized and raw differences between
#' consecutive iterations to evaluate stability and convergence.
#'
#' @param mcmodule A Monte Carlo module object containing simulation results
#' @param from_quantile Lower bound quantile for analysis (default: 0.95)
#' @param to_quantile Upper bound quantile for analysis (default: 1)
#' @param conv_threshold Optional custom convergence threshold for standardized differences
#'
#' @return A data frame with convergence statistics per node:
#'   \itemize{
#'     \item expression: Expression identifier
#'     \item variate: Variate identifier
#'     \item node: Node identifier
#'     \item max_std_dif: Max of standardized differences
#'     \item max_dif: Max of raw differences
#'     \item conv_threshold: Convergence at custom threshold (TRUE/FALSE), if threshold provided
#'     \item conv_01: Convergence at 1% std threshold (TRUE/FALSE)
#'     \item conv_025: Convergence at 2.5% std threshold (TRUE/FALSE)
#'     \item conv_05: Convergence at 5% std threshold (TRUE/FALSE)
#'   }
#'
#' @details
#' The function performs the following:
#' \itemize{
#'   \item Calculates convergence statistics for specified quantile range
#'   \item Generates diagnostic plots for standardized and raw differences
#'   \item Provides detailed convergence summary including:
#'     \itemize{
#'       \item Total nodes analyzed
#'       \item Number and percentage of nodes converged at different thresholds
#'       \item Maximum/minimum deviations
#'       \item List of non-converged nodes (if any)
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' results <- mcmodule_converg(mc_results)
#' results <- mcmodule_converg(mc_results, from_quantile = 0.90, conv_threshold = 0.01)
#' }
#'
#' @export
mcmodule_converg <- function(mcmodule, from_quantile = 0.95, to_quantile = 1, conv_threshold = NULL) {
  # Helper function to calculate statistics (mean and quantiles) for convergence analysis
  mc_stat <- function(i, x) {
    c(
      mean = mean(x[1:i], na.rm = TRUE),
      quantile(x[1:i], probs = c(0.5, 0.025, 0.975), na.rm = TRUE, names = FALSE)
    )
  }

  # Calculate total iterations for progress bar
  total_iterations <- sum(sapply(mcmodule$mc_list, function(mc) {
    sum(sapply(mc, function(var) length(var)))
  }))
  pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)
  current <- 0

  # Initialize list to store convergence results
  mc_convergence_list <- vector("list", total_iterations)
  list_index <- 1

  # Iterate through each expression in the Monte Carlo module
  for (i in seq_along(mcmodule$mc_list)) {
    expression <- names(mcmodule$mc_list)[i]
    mc_i <- mcmodule$mc_list[[i]]

    # Process each variate within the expression
    for (j in seq_along(mc_i)) {
      variate <- j
      mc_j <- mc_i[[j]]

      # Analyze convergence for each node
      for (k in seq_along(mc_j)) {
        node <- names(mc_j)[k]
        x <- mc_j[[k]]

        # Only analyze nodes with more than one iteration and that have variability/uncertainty
        if (dim(x)[1] > 1 & !max(x) == min(x)) {
          # Calculate convergence statistics for the specified quantile range
          conv_start <- floor(dim(x)[1] * from_quantile)
          conv_end <- floor(dim(x)[1] * to_quantile)
          n_sim <- dim(x)[1]
          n_sim_conv <- conv_end - conv_start

          x_conv <- vapply(conv_start:conv_end, mc_stat, x = x, FUN.VALUE = numeric(4))

          # Calculate differences between iterations
          x_conv_dif <- x_conv - cbind(0, x_conv[, 1:(ncol(x_conv) - 1)])
          x_conv_dif <- x_conv_dif[, -1]

          # Calculate convergence metrics
          max_dif <- max(x_conv_dif)
          mean_value <- mean(x_conv[1, ])
          max_dif_scaled <- max_dif / mean_value

          if (!is.null(conv_threshold)) {
            conv_manual <- abs(max_std_dif) < conv_threshold
          }

          tiny <- max_dif < 0.001

          conv_01 <- abs(max_dif_scaled) < 0.01
          conv_025 <- abs(max_dif_scaled) < 0.025
          conv_05 <- abs(max_dif_scaled) < 0.05

          conv_01_tiny <- conv_01 | tiny
          conv_025_tiny <- conv_025 | tiny
          conv_05_tiny <- conv_05 | tiny

          if (!is.na(max_dif) && !is.na(max_dif_scaled)) {
            mc_convergence_list[[list_index]] <- data.frame(
              expression = expression,
              variate = variate,
              node = node,
              mean_value = mean_value,
              max_dif = max_dif,
              max_dif_scaled = max_dif_scaled,
              conv_manual = if (!is.null(conv_threshold)) conv_manual else NA,
              conv_01 = conv_01,
              conv_025 = conv_025,
              conv_05 = conv_05,
              tiny = tiny,
              conv_01_tiny = conv_01_tiny,
              conv_025_tiny = conv_025_tiny,
              conv_05_tiny = conv_05_tiny
            )

            list_index <- list_index + 1
          }
        }

        # Update progress bar
        current <- current + 1
        setTxtProgressBar(pb, current)
      }
    }
  }

  # Combine all results and return
  conv_df <- do.call(rbind, mc_convergence_list[1:(list_index - 1)])

  # Set up 2x1 plotting layout
  par(mfrow = c(2, 1))

  # Plot standardized differences
  plot(conv_df$max_dif_scaled,
    type = "l",
    xlab = "MC Node Variate",
    ylab = "Max Difference/Mean",
    main = "Convergence Analysis - Scaled"
  )
  abline(h = mean(conv_df$max_dif_scaled), lty = 2, col = "gray")

  # Plot real differences
  plot(conv_df$max_dif,
    type = "l",
    xlab = "MC Node Variate",
    ylab = "Max Difference",
    main = "Convergence Analysis - Real Values"
  )
  abline(h = mean(conv_df$max_dif), lty = 2, col = "gray")

  # Reset plotting parameters to default
  par(mfrow = c(1, 1))

  # Print analysis results summary
  cat("\n=== Convergence Analysis Summary ===\n")
  cat("\nAnalysis Parameters:")
  cat("\n- Number of simulations:", n_sim)
  cat("\n- Simulation quantile range:", from_quantile, "to", to_quantile)
  cat("\n- Simulations range:", conv_start, "to", conv_end, paste0("(", n_sim_conv, " simulations)"))
  if (!is.null(conv_threshold)) {
    cat("\n- Custom convergence threshold:", conv_threshold)
  }

  # Calculate convergence statistics
  total_nodes <- nrow(conv_df)

  cat("\n\nConvergence Results:")
  cat("\n- Total nodes analyzed:", total_nodes)
  if (!is.null(conv_threshold)) {
    converged_manual <- sum(conv_df$conv_manual, na.rm = TRUE)
    cat(sprintf(
      "\n- Nodes converged at %.4f threshold: %d (%.2f%%)",
      conv_threshold, converged_manual, converged_manual / total_nodes * 100
    ))
  }

  n_tiny <- sum(conv_df$tiny)

  converged_01 <- sum(conv_df$conv_01_tiny)
  converged_025 <- sum(conv_df$conv_025_tiny)
  converged_05 <- sum(conv_df$conv_05_tiny)

  no_converged_05 <- total_nodes - converged_05

  cat(sprintf(
    "\n- Nodes diverge less than 0.001: %d (%.2f%%)",
    n_tiny, n_tiny / total_nodes * 100
  ))

  cat(sprintf(
    "\n- Nodes diverge less than 0.001 or 1%% of their mean: %d (%.2f%%)",
    converged_01, converged_01 / total_nodes * 100
  ))

  cat(sprintf(
    "\n- Nodes diverge less than 0.001 or 2.5%% of their mean: %d (%.2f%%)",
    converged_025, converged_025 / total_nodes * 100
  ))

  cat(sprintf(
    "\n- Nodes diverge less than 0.001 or 5%% of their mean: %d (%.2f%%)",
    converged_05, converged_05 / total_nodes * 100
  ))

  # Print deviation statistics
  cat("\n\nDeviation Statistics:")
  cat("\nRaw differences:")
  cat(sprintf("\n- Maximum deviation: %.6f", max(abs(conv_df$max_dif))))
  cat(sprintf("\n- Mean maximum deviation: %.6f", mean(abs(conv_df$max_dif))))
  cat("\nScaled differences (Deviation/Mean):")
  cat(sprintf("\n- Maximum deviation: %.6f", max(abs(conv_df$max_dif_scaled))))
  cat(sprintf("\n- Mean maximum deviation: %.6f", mean(abs(conv_df$max_dif_scaled))))

  # Happy message if all converged at 5% threshold
  if (converged_01 == total_nodes) {
    cat("\n\nAll nodes successfully converged at 1% threshold! :D\n")
  } else if (converged_025 == total_nodes) {
    cat("\n\nAll nodes successfully converged at 2.5% threshold! :)\n")
  } else if (converged_05 == total_nodes) {
    cat("\n\nAll nodes successfully converged at 5% threshold! :)\n")
  } else {
    cat(sprintf(
      "\n\n%d (%.2f%%) nodes did not converge at 5%% threshold :(",
      no_converged_05, (no_converged_05 / total_nodes) * 100
    ))
  }

  return(conv_df)
}
