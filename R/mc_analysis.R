#' Check Monte Carlo Nodes Subset
#'
#' Validates that all Monte Carlo nodes in a module have compatible dimensions
#' for sensitivity analysis by checking uncertainty and variate dimensions.
#'
#' @param mcmodule A Monte Carlo module object
#' @param mc_names Optional character vector of Monte Carlo node names to check.
#'   If NULL (default), checks all nodes in the module.
#' @return A list with three elements:
#'   \item{n_mcnodes}{Number of Monte Carlo nodes}
#'   \item{n_variate}{Number of variate simulations}
#'   \item{n_uncertainty}{Number of uncertainty simulations}
mcmodule_dim_check <- function(mcmodule, mc_names = NULL) {
  mc_names <- mc_names %||% names(mcmodule$node_list)

  # Check that all mcnodes have 1 or the same number of uncertainty values
  n_uncertainty <- unique(sapply(mc_names, \(x) {
    dim(mcmodule$node_list[[x]][["mcnode"]])[1]
  }))
  n_uncertainty <- n_uncertainty[n_uncertainty != 1]

  if (length(unique(n_uncertainty)) > 1) {
    stop(
      "All mcnode objects must have the same number of uncertanty simulations or no uncertainty."
    )
  }

  # Check that all mcnodes have 1 or the same number of variate simulations
  n_variate <- unique(sapply(mc_names, \(x) {
    dim(mcmodule$node_list[[x]][["mcnode"]])[3]
  }))
  n_variate <- n_variate[n_variate != 1]
  if (length(unique(n_variate)) > 1) {
    stop(
      "All mcnode objects must have the same number of variate simulations or one variate"
    )
  }

  list(
    n_mcnodes = length(mc_names),
    n_variate = ifelse(length(n_variate) == 0, 1, n_variate),
    n_uncertainty = ifelse(length(n_uncertainty) == 0, 1, n_uncertainty)
  )
}

#' Convert Monte Carlo Module to Matrices
#'
#' Transforms a Monte Carlo module into a list of matrices, with one matrix per
#' variate simulation. Each matrix has uncertainty simulations as rows and
#' Monte Carlo nodes as columns.
#'
#' @param mcmodule A Monte Carlo module object
#' @param mc_names Optional character vector of Monte Carlo node names to include.
#'   If NULL (default), includes all nodes in the module.
#' @return A list of matrices, one per variate simulation. Each matrix has:
#'   \itemize{
#'     \item Rows: uncertainty simulations
#'     \item Columns: Monte Carlo nodes (in order of mc_names)
#'   }
mcmodule_to_matrices <- function(mcmodule, mc_names = NULL) {
  mc_names <- mc_names %||% names(mcmodule$node_list)
  dims <- mcmodule_dim_check(mcmodule, mc_names)
  # Initialize list to store matrices: one per n_variate
  matrices <- vector("list", dims$n_variate)
  # Intitialize matrices (n_uncertainty x n_mcnodes)
  matrices <- lapply(matrices, function(x) {
    matrix(nrow = dims$n_uncertainty, ncol = dims$n_mcnodes)
  })

  for (i in seq_along(mc_names)) {
    mcnode_i <- mcmodule$node_list[[mc_names[i]]][["mcnode"]]
    for (j in seq_len(dim(mcnode_i)[3])) {
      variate_i_j <- mcnode_i[,, j]

      if (length(variate_i_j) == 1) {
        variate_i_j <- rep(variate_i_j, dims$n_uncertainty)
      }

      matrices[[j]][, i] <- variate_i_j
    }
  }
  matrices
}

#' Convert Monte Carlo Module to mc2d mc Objects
#'
#' Transforms a Monte Carlo module into a list of mc objects (from the mc2d package),
#' with one mc object per variate simulation.
#'
#' @param mcmodule A Monte Carlo module object
#' @param mc_names Optional character vector of Monte Carlo node names to include.
#'   If NULL (default), includes all nodes in the module.
#' @param match Logical, currently unused (reserved for future functionality)
#' @return A list of mc objects (one per variate) combining specified Monte Carlo nodes.
#'   Each mc object is compatible with mc2d package functions.
mcmodule_to_mc <- function(mcmodule, mc_names = NULL, match = FALSE) {
  mc_names <- mc_names %||% names(mcmodule$node_list)
  dims <- mcmodule_dim_check(mcmodule, mc_names)
  mc_list <- vector("list", dims$n_variate)

  for (j in seq_len(dims$n_variate)) {
    mc_j <- NULL
    for (i in seq_along(mc_names)) {
      mcnode_i <- mcmodule$node_list[[mc_names[i]]][["mcnode"]]
      variate_i_j <- extractvar(mcnode_i, j)

      if (is.null(mc_j)) {
        mc_j <- mc(variate_i_j, name = mc_names[i])
      } else {
        mc_j <- mc(mc_j, mc(variate_i_j, name = mc_names[i]))
      }
    }
    mc_list[[j]] <- mc_j
  }
  mc_list
}

#' Get Monte Carlo Module Index Information
#'
#' Extracts metadata about a Monte Carlo module, including input data per expression,
#' keys for each variate (data row), and global data keys.
#'
#' @param mcmodule A Monte Carlo module object
#' @return A list with three elements:
#'   \item{module_exp_data}{Data frame with module and expression information, including data_name}
#'   \item{data_keys}{Data frame with keys for each variate, including variate number and data_name}
#'   \item{global_keys}{Character vector of global key names used across the module}
#' @export
mcmodule_index <- function(mcmodule) {
  # Extract module expressions and metadata
  exp_name <- unlist(lapply(names(mcmodule$node_list), \(x) {
    mcmodule$node_list[[x]][["exp_name"]] %||% NA
  }))

  data_name <- unlist(lapply(names(mcmodule$node_list)[!is.na(exp_name)], \(x) {
    mcmodule$node_list[[x]][["data_name"]] %||% NA
  }))
  names(data_name) <- exp_name[!is.na(exp_name)]
  data_name <- data_name[!is.na(data_name) & !duplicated(data_name)]

  module_exp <- mcmodule_composition(mcmodule)$module_exp

  # Create a data frame to store module, expression and data_name
  module_exp$data_name <- data_name[module_exp$exp]

  # Process global keys
  global_keys <- unique(unlist(lapply(names(mcmodule$node_list), \(x) {
    mcmodule$node_list[[x]][["keys"]]
  })))

  # Process data keys
  data_keys <- data.frame()
  for (i in unique(data_name)[unique(data_name) %in% names(mcmodule$data)]) {
    data_i <- mcmodule$data[[i]][names(mcmodule$data[[i]]) %in% global_keys]
    data_i$variate <- seq_len(nrow(data_i))
    data_i$data_name <- i
    data_keys <- bind_rows(data_keys, data_i)
  }

  list(
    module_exp_data = module_exp,
    data_keys = data_keys,
    global_keys = global_keys
  )
}

#' Calculate Correlation Coefficients for Monte Carlo Module Inputs and Outputs
#'
#' Computes correlation coefficients between Monte Carlo module inputs and outputs
#' using the tornado analysis from the mc2d package. Supports multiple correlation
#' methods and captures any warnings generated during calculation.
#'
#' @param mcmodule Monte Carlo module object
#' @param by_exp Logical, whether to calculate correlations by expression output.
#'   If FALSE (default), calculates correlation by global output (last node).
#' @param output Character string specifying the output node name. If NULL (default),
#'   uses the last node in mcmodule$node_list (or last expression output if by_exp = TRUE).
#' @param all_variates Logical, whether to match input nodes to output variates.
#'   Default is TRUE.
#' @inheritParams mc2d::tornado
#' @return Data frame with correlation coefficients and metadata. Columns include:
#'   \itemize{
#'     \item exp: Expression name
#'     \item exp_n: Expression number
#'     \item variate: Variate number
#'     \item output: Output node name
#'     \item input: Input node name
#'     \item value: Correlation coefficient value
#'     \item method: Correlation method used (spearman, kendall, or pearson)
#'     \item use: Method for handling missing values (passed to cor())
#'     \item warnings: Any warnings generated during correlation calculation (if present)
#'     \item Additional columns for global keys (e.g., pathogen, origin)
#'   }
#' @export
#'
#' @examples
#' mcmodule <- agg_totals(mcmodule = imports_mcmodule, mc_name = "no_detect_a", agg_keys = "pathogen")
#' cor_results <- mcmodule_corr(mcmodule)
#'
#' # Use single method
#' cor_results_spearman <- mcmodule_corr(mcmodule, method = "spearman")
#'
mcmodule_corr <- function(
  mcmodule,
  output = NULL,
  by_exp = FALSE,
  all_variates = TRUE,
  method = c("spearman", "kendall", "pearson"),
  use = "all.obs",
  lim = c(0.025, 0.975)
) {
  index <- mcmodule_index(mcmodule)
  module_names <- unique(index$module_exp_data$module)

  # Initialize correlation data frame
  coor <- data.frame(
    exp = character(),
    exp_n = integer(),
    variate = integer(),
    output = character(),
    input = character(),
    value = numeric(),
    method = character(),
    use = character()
  )

  # Process each module
  for (h in seq_along(module_names)) {
    exp_h <- index$module_exp_data$exp[
      index$module_exp_data$module == module_names[h]
    ]
    # Get input (type == "in_node") mcnodes names in mcmodule$node_list for this expression (module == exp_h)
    exp_h_inputs <- names(mcmodule$node_list)[
      unlist(lapply(names(mcmodule$node_list), \(x) {
        mcmodule$node_list[[x]][["exp_name"]] %in%
          exp_h &&
          mcmodule$node_list[[x]][["type"]] == "in_node"
      }))
    ]

    if (is.null(output)) {
      if (by_exp) {
        exp_h_outputs <- names(mcmodule$node_list)[
          unlist(lapply(names(mcmodule$node_list), \(x) {
            mcmodule$node_list[[x]][["exp_name"]] %in%
              exp_h &&
              mcmodule$node_list[[x]][["type"]] == "out_node"
          }))
        ]
        output_h <- exp_h_outputs[length(exp_h_outputs)]
      } else {
        output_h <- names(mcmodule$node_list)[length(mcmodule$node_list)]
      }
    } else {
      output_h <- output
    }

    # Get output
    mc_output <- mcmodule$node_list[[output_h]][["mcnode"]]
    summary_output <- mcmodule$node_list[[output_h]][["summary"]]

    data_name_h <- index$module_exp_data$data_name[
      index$module_exp_data$exp == exp_h
    ]

    suppressMessages({
      mc_match_data_h <- mc_match_data(
        mcmodule,
        output_h,
        mcmodule$data[[data_name_h]]
      )
    })

    mc_output_h <- mc_match_data_h[[1]]
    data_h <- mc_match_data_h[[2]]
    keys_h <- mc_match_data_h[[3]]

    # Check data and keys are compatible
    if (
      !all(
        keys_h[intersect(names(keys_h), names(data_h))] ==
          data_h[intersect(names(keys_h), names(data_h))]
      )
    ) {
      stop(paste0(
        "Data and keys are not compatible for expression '",
        exp_h,
        "'"
      ))
    }

    # Create a copy of mcmodule to modify
    mcmodule_h <- mcmodule

    # Match input mcnodes to output mcnode if all_variates is TRUE and data dimensions differ
    if (
      all_variates &&
        (!all(dim(data_h) == dim(mcmodule_h$data[[data_name_h]])) ||
          !all(data_h == mcmodule_h$data[[data_name_h]]))
    ) {
      for (input_name in exp_h_inputs) {
        mc_input <- mcmodule_h$node_list[[input_name]][["mcnode"]]
        suppressMessages({
          mc_input_matched <- mc_match_data(
            mcmodule_h,
            input_name,
            data_h,
            keys_names = intersect(names(data_h), index$global_keys)
          )[[1]]
          mc_input_matched
          mcmodule_h$node_list[[input_name]][["mcnode"]] <- mc_input_matched
        })
      }
    }

    # Temporarily replace output mcnode with matched output
    mcmodule_h$node_list[[output_h]][["mcnode"]] <- mc_output_h

    # Convert mcmodule to mc object with only inputs and output
    mc_h <- mcmodule_to_mc(
      mcmodule = mcmodule_h,
      mc_names = c(exp_h_inputs, output_h)
    )

    # Calculate correlation for this expression and variate
    for (i in seq_along(mc_h)) {
      tornado_result <- local({
        warnings <- character()

        tornado_h_i <- tryCatch(
          withCallingHandlers(
            tornado(mc_h[[i]], output = output_h, method = method),
            warning = function(w) {
              warnings <<- c(warnings, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            warnings <<- c(warnings, paste("Error:", conditionMessage(e)))
            NULL
          }
        )

        list(tornado = tornado_h_i, warnings = warnings)
      })

      tornado_h_i <- tornado_result$tornado

      if (is.null(tornado_h_i)) {
        names_h_i <- exp_h_inputs
        values_h_i <- NA
      } else {
        names_h_i <- c(colnames(tornado_h_i[[1]][[1]]))
        values_h_i <- unlist(tornado_h_i[1])
      }

      coor_h_i <- data.frame(
        input = names_h_i,
        value = values_h_i,
        output = output_h,
        method = tornado_h_i$method,
        use = tornado_h_i$use,
        row.names = NULL
      )

      coor_h_i$variate <- i
      coor_h_i$exp <- exp_h
      coor_h_i$exp_n <- h
      coor_h_i[intersect(names(data_h), index$global_keys)] <- data_h[
        i,
        intersect(names(data_h), index$global_keys)
      ]

      if (length(tornado_result$warnings) > 0) {
        coor_h_i$warnings <- paste(tornado_result$warnings, collapse = "; ")
      }

      coor <- bind_rows(coor, coor_h_i)
    }
  }
  #Move warnings column to the end if it exists base R
  if ("warnings" %in% names(coor)) {
    coor <- coor[, c(setdiff(names(coor), "warnings"), "warnings")]
  }
  coor
}

# # TODO!!
# #' Calculate Relative Change in Monte Carlo Module
# #'
# #' @param mcmodule_def Default mcmoduleduction module
# #' @param mcmodule_alt Alternative mcmoduleduction module
# #' @param mc_names Optional names of Monte Carlo nodes
# #' @param mcnode_admin Monte Carlo node administration object
# #' @return Modified mcmoduleduction module with relative changes
# #' @export
# mcmodule_rel_change <- function(
#   mcmodule_def,
#   mcmodule_alt,
#   mc_names = NULL,
#   mcnode_admin = mcnode_admin
# ) {
#   mcmodule_rel <- mcmodule_def
#   mc_names <- mc_names %||% names(mcmodule_rel$node_list)
#   mcmodule_rel$node_list <- mcmodule_rel$node_list[mc_names]

#   pb <- txtProgressBar(min = 0, max = length(mc_names), initial = 0, style = 3)

#   for (i in seq_along(mc_names)) {
#     mcnode_def <- mcmodule_def$node_list[[mc_names[i]]][["mcnode"]]
#     mcnode_alt <- mcmodule_alt$node_list[[mc_names[i]]][["mcnode"]]

#     if (!typemcnode(mcnode_def) == "0") {
#       mcnode_cor <- cornode(
#         mcnode_def,
#         mcnode_alt,
#         target = 0.9999999,
#         outrank = FALSE,
#         result = FALSE
#       )
#       mcnode_def <- mcnode_cor[[1]]
#       mcnode_alt <- mcnode_cor[[2]]
#     }

#     # Calculate relative change
#     mcnode_rel <- mcnode_na_rm((mcnode_alt - mcnode_def) / mcnode_def)
#     mcmodule_rel$node_list[[mc_names[i]]][["mcnode"]] <- mcnode_rel

#     # Update summary if it exists
#     if (!is.null(mcmodule_rel$node_list[[mc_names[i]]][["summary"]])) {
#       summary_rel_l <- summary(mcnode_rel)[[1]]
#       if (!is.list(summary_rel_l)) {
#         summary_rel_l <- list(summary_rel_l)
#       }

#       summary_names <- colnames(summary_rel_l[[1]])
#       summary_rel <- data.frame(matrix(
#         unlist(summary_rel_l),
#         nrow = length(summary_rel_l),
#         byrow = TRUE
#       ))
#       names(summary_rel) <- summary_names

#       summary_def <- mcmodule_rel$node_list[[mc_names[i]]][["summary"]]
#       summary_def[summary_names] <- NULL
#       mcmodule_rel$node_list[[mc_names[i]]][["summary"]] <- cbind(
#         summary_def,
#         summary_rel
#       )
#     }

#     setTxtProgressBar(pb, i)
#   }

#   close(pb)
#   mcmodule_rel
# }

# #' Monte Carlo Simulation Convergence Analysis
# #'
# #' @description
# #' Analyzes convergence in Monte Carlo simulations by computing statistical measures
# #' across iterations. Calculates both standardized and raw differences between
# #' consecutive iterations to evaluate stability and convergence.
# #'
# #' @param mcmodule A Monte Carlo module object containing simulation results
# #' @param from_quantile Lower bound quantile for analysis (default: 0.95)
# #' @param to_quantile Upper bound quantile for analysis (default: 1)
# #' @param conv_threshold Optional custom convergence threshold for standardized differences
# #'
# #' @return A data frame with convergence statistics per node:
# #'   \itemize{
# #'     \item expression: Expression identifier
# #'     \item variate: Variate identifier
# #'     \item node: Node identifier
# #'     \item max_std_dif: Max of standardized differences
# #'     \item max_dif: Max of raw differences
# #'     \item conv_threshold: Convergence at custom threshold (TRUE/FALSE), if threshold provided
# #'     \item conv_01: Convergence at 1% std threshold (TRUE/FALSE)
# #'     \item conv_025: Convergence at 2.5% std threshold (TRUE/FALSE)
# #'     \item conv_05: Convergence at 5% std threshold (TRUE/FALSE)
# #'   }
# #'
# #' @details
# #' The function performs the following:
# #' \itemize{
# #'   \item Calculates convergence statistics for specified quantile range
# #'   \item Generates diagnostic plots for standardized and raw differences
# #'   \item Provides detailed convergence summary including:
# #'     \itemize{
# #'       \item Total nodes analyzed
# #'       \item Number and percentage of nodes converged at different thresholds
# #'       \item Maximum/minimum deviations
# #'       \item List of non-converged nodes (if any)
# #'     }
# #' }
# #'
# #' @examples
# #' \dontrun{
# #' results <- mcmodule_converg(mc_results)
# #' results <- mcmodule_converg(mc_results, from_quantile = 0.90, conv_threshold = 0.01)
# #' }
# #'
# #' @export
# mcmodule_converg <- function(
#   mcmodule,
#   from_quantile = 0.95,
#   to_quantile = 1,
#   conv_threshold = NULL
# ) {
#   # Helper function to calculate statistics (mean and quantiles) for convergence analysis
#   mc_stat <- function(i, x) {
#     c(
#       mean = mean(x[1:i], na.rm = TRUE),
#       quantile(
#         x[1:i],
#         probs = c(0.5, 0.025, 0.975),
#         na.rm = TRUE,
#         names = FALSE
#       )
#     )
#   }

#   # Calculate total iterations for progress bar
#   total_iterations <- sum(sapply(mcmodule$mc_list, function(mc) {
#     sum(sapply(mc, function(var) length(var)))
#   }))
#   pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)
#   current <- 0

#   # Initialize list to store convergence results
#   mc_convergence_list <- vector("list", total_iterations)
#   list_index <- 1

#   # Iterate through each expression in the Monte Carlo module
#   for (i in seq_along(mcmodule$mc_list)) {
#     expression <- names(mcmodule$mc_list)[i]
#     mc_i <- mcmodule$mc_list[[i]]

#     # Process each variate within the expression
#     for (j in seq_along(mc_i)) {
#       variate <- j
#       mc_j <- mc_i[[j]]

#       # Analyze convergence for each node
#       for (k in seq_along(mc_j)) {
#         node <- names(mc_j)[k]
#         x <- mc_j[[k]]

#         # Only analyze nodes with more than one iteration and that have variability/uncertainty
#         if (dim(x)[1] > 1 & !max(x) == min(x)) {
#           # Calculate convergence statistics for the specified quantile range
#           conv_start <- floor(dim(x)[1] * from_quantile)
#           conv_end <- floor(dim(x)[1] * to_quantile)
#           n_sim <- dim(x)[1]
#           n_sim_conv <- conv_end - conv_start

#           x_conv <- vapply(
#             conv_start:conv_end,
#             mc_stat,
#             x = x,
#             FUN.VALUE = numeric(4)
#           )

#           # Calculate differences between iterations
#           x_conv_dif <- x_conv - cbind(0, x_conv[, 1:(ncol(x_conv) - 1)])
#           x_conv_dif <- x_conv_dif[, -1]

#           # Calculate convergence metrics
#           max_dif <- max(x_conv_dif)
#           mean_value <- mean(x_conv[1, ])
#           max_dif_scaled <- max_dif / mean_value

#           if (!is.null(conv_threshold)) {
#             conv_manual <- abs(max_std_dif) < conv_threshold
#           }

#           tiny <- max_dif < 0.001

#           conv_01 <- abs(max_dif_scaled) < 0.01
#           conv_025 <- abs(max_dif_scaled) < 0.025
#           conv_05 <- abs(max_dif_scaled) < 0.05

#           conv_01_tiny <- conv_01 | tiny
#           conv_025_tiny <- conv_025 | tiny
#           conv_05_tiny <- conv_05 | tiny

#           if (!is.na(max_dif) && !is.na(max_dif_scaled)) {
#             mc_convergence_list[[list_index]] <- data.frame(
#               expression = expression,
#               variate = variate,
#               node = node,
#               mean_value = mean_value,
#               max_dif = max_dif,
#               max_dif_scaled = max_dif_scaled,
#               conv_manual = if (!is.null(conv_threshold)) conv_manual else NA,
#               conv_01 = conv_01,
#               conv_025 = conv_025,
#               conv_05 = conv_05,
#               tiny = tiny,
#               conv_01_tiny = conv_01_tiny,
#               conv_025_tiny = conv_025_tiny,
#               conv_05_tiny = conv_05_tiny
#             )

#             list_index <- list_index + 1
#           }
#         }

#         # Update progress bar
#         current <- current + 1
#         setTxtProgressBar(pb, current)
#       }
#     }
#   }

#   # Combine all results and return
#   conv_df <- do.call(rbind, mc_convergence_list[1:(list_index - 1)])

#   # Set up 2x1 plotting layout
#   par(mfrow = c(2, 1))

#   # Plot standardized differences
#   plot(
#     conv_df$max_dif_scaled,
#     type = "l",
#     xlab = "MC Node Variate",
#     ylab = "Max Difference/Mean",
#     main = "Convergence Analysis - Scaled"
#   )
#   abline(h = mean(conv_df$max_dif_scaled), lty = 2, col = "gray")

#   # Plot real differences
#   plot(
#     conv_df$max_dif,
#     type = "l",
#     xlab = "MC Node Variate",
#     ylab = "Max Difference",
#     main = "Convergence Analysis - Real Values"
#   )
#   abline(h = mean(conv_df$max_dif), lty = 2, col = "gray")

#   # Reset plotting parameters to default
#   par(mfrow = c(1, 1))

#   # Print analysis results summary
#   cat("\n=== Convergence Analysis Summary ===\n")
#   cat("\nAnalysis Parameters:")
#   cat("\n- Number of simulations:", n_sim)
#   cat("\n- Simulation quantile range:", from_quantile, "to", to_quantile)
#   cat(
#     "\n- Simulations range:",
#     conv_start,
#     "to",
#     conv_end,
#     paste0("(", n_sim_conv, " simulations)")
#   )
#   if (!is.null(conv_threshold)) {
#     cat("\n- Custom convergence threshold:", conv_threshold)
#   }

#   # Calculate convergence statistics
#   total_nodes <- nrow(conv_df)

#   cat("\n\nConvergence Results:")
#   cat("\n- Total nodes analyzed:", total_nodes)
#   if (!is.null(conv_threshold)) {
#     converged_manual <- sum(conv_df$conv_manual, na.rm = TRUE)
#     cat(sprintf(
#       "\n- Nodes converged at %.4f threshold: %d (%.2f%%)",
#       conv_threshold,
#       converged_manual,
#       converged_manual / total_nodes * 100
#     ))
#   }

#   n_tiny <- sum(conv_df$tiny)

#   converged_01 <- sum(conv_df$conv_01_tiny)
#   converged_025 <- sum(conv_df$conv_025_tiny)
#   converged_05 <- sum(conv_df$conv_05_tiny)

#   no_converged_05 <- total_nodes - converged_05

#   cat(sprintf(
#     "\n- Nodes diverge less than 0.001: %d (%.2f%%)",
#     n_tiny,
#     n_tiny / total_nodes * 100
#   ))

#   cat(sprintf(
#     "\n- Nodes diverge less than 0.001 or 1%% of their mean: %d (%.2f%%)",
#     converged_01,
#     converged_01 / total_nodes * 100
#   ))

#   cat(sprintf(
#     "\n- Nodes diverge less than 0.001 or 2.5%% of their mean: %d (%.2f%%)",
#     converged_025,
#     converged_025 / total_nodes * 100
#   ))

#   cat(sprintf(
#     "\n- Nodes diverge less than 0.001 or 5%% of their mean: %d (%.2f%%)",
#     converged_05,
#     converged_05 / total_nodes * 100
#   ))

#   # Print deviation statistics
#   cat("\n\nDeviation Statistics:")
#   cat("\nRaw differences:")
#   cat(sprintf("\n- Maximum deviation: %.6f", max(abs(conv_df$max_dif))))
#   cat(sprintf("\n- Mean maximum deviation: %.6f", mean(abs(conv_df$max_dif))))
#   cat("\nScaled differences (Deviation/Mean):")
#   cat(sprintf("\n- Maximum deviation: %.6f", max(abs(conv_df$max_dif_scaled))))
#   cat(sprintf(
#     "\n- Mean maximum deviation: %.6f",
#     mean(abs(conv_df$max_dif_scaled))
#   ))

#   # Happy message if all converged at 5% threshold
#   if (converged_01 == total_nodes) {
#     cat("\n\nAll nodes successfully converged at 1% threshold! :D\n")
#   } else if (converged_025 == total_nodes) {
#     cat("\n\nAll nodes successfully converged at 2.5% threshold! :)\n")
#   } else if (converged_05 == total_nodes) {
#     cat("\n\nAll nodes successfully converged at 5% threshold! :)\n")
#   } else {
#     cat(sprintf(
#       "\n\n%d (%.2f%%) nodes did not converge at 5%% threshold :(",
#       no_converged_05,
#       (no_converged_05 / total_nodes) * 100
#     ))
#   }

#   return(conv_df)
# }
