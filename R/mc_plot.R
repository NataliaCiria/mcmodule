#' Convert mcnode to Long Format for Plotting
#'
#' Converts Monte Carlo node data to a long format data frame suitable for use
#' with ggplot2 and other tidyverse functions. Each row represents a single Monte Carlo
#' simulation for a specific variate (scenario/data row).
#'
#' @param mcmodule An mcmodule object containing the node (optional if mcnode provided)
#' @param mc_name Character string specifying the name of the mcnode in the module (optional)
#' @param mcnode An mcnode object to convert directly (optional if mcmodule and mc_name provided)
#' @param data Optional data frame containing the input data. If NULL and mcmodule provided,
#'   data will be extracted from the module
#' @param keys_names Vector of column names to use as key columns for grouping (optional).
#'   If NULL, will use node keys from the module or all available keys
#'
#' @return A long format data frame with columns:
#'   - All key columns (from keys_names)
#'   - variate: Variate index (1 to number of data rows/scenarios)
#'   - simulation: Simulation index (1 to number of uncertainty iterations)
#'   - value: The Monte Carlo node value
#'
#' @details
#' This function can be called in multiple ways:
#' 1. With mcmodule and mc_name: `tidy_mcnode(mcmodule, "w_prev")`
#' 2. With mcnode and data: `tidy_mcnode(mcnode = mcnode, data = data)`
#' 3. With mcmodule and mcnode: `tidy_mcnode(mcmodule, mcnode = mcnode)`
#'
#' @examples
#' # Using mcmodule and mc_name
#' long_data <- tidy_mcnode(imports_mcmodule, "w_prev")
#'
#' # Using with keys_names parameter
#' long_data <- tidy_mcnode(imports_mcmodule, "w_prev",
#'   keys_names = c("origin")
#' )
#'
#' # Using mcnode and data directly
#' w_prev <- imports_mcmodule$node_list$w_prev$mcnode
#' long_data <- tidy_mcnode(mcnode = w_prev, data = imports_data)
#'
#' @export
tidy_mcnode <- function(
  mcmodule = NULL,
  mc_name = NULL,
  mcnode = NULL,
  data = NULL,
  keys_names = NULL
) {
  # Input validation and setup
  if (!is.null(mcnode) && is.null(mc_name)) {
    mc_name <- deparse(substitute(mcnode))
  }

  if (!is.null(mcmodule)) {
    module_name <- deparse(substitute(mcmodule))

    if (is.null(mcnode)) {
      mcnode <- mcmodule$node_list[[mc_name]]$mcnode
    }

    if (!is.mcnode(mcnode)) {
      stop(sprintf("%s must be a mcnode present in %s", mc_name, module_name))
    }

    data_name <- mcmodule$node_list[[mc_name]]$data_name

    if (is.null(data)) {
      data <- mcmodule$data[[data_name]]
    }
  } else {
    if (is.null(data)) {
      stop("mcmodule or data must be provided")
    }
  }

  # Validate provided keys
  if (!is.null(keys_names)) {
    missing_keys <- keys_names[!keys_names %in% names(data)]
    if (length(missing_keys) > 0) {
      stop(sprintf(
        "keys_names (%s) must appear in data column names",
        paste(missing_keys, collapse = ", ")
      ))
    }
  }

  # Determine keys to use
  if (is.null(keys_names) && !is.null(mcmodule)) {
    keys_names <- mcmodule$node_list[[mc_name]]$keys
  }

  # Extract key columns from data
  if (!is.null(keys_names) && length(keys_names) > 0) {
    keys_df <- data[names(data) %in% keys_names]
  } else {
    keys_df <- data.frame(row_id = seq_len(nrow(data)))
  }

  # Get mcnode dimensions
  # Dimension 1: uncertainty iterations
  # Dimension 2: variability iterations (usually 1)
  # Dimension 3: variates (scenarios/rows from data)
  dims <- dim(mcnode)
  n_uncertainty <- dims[1]
  n_variability <- dims[2]
  n_variates <- dims[3]

  # Check that number of variates matches number of data rows
  if (n_variates != nrow(keys_df)) {
    stop(sprintf(
      "Mismatch: mcnode has %d variates but data has %d rows",
      n_variates,
      nrow(keys_df)
    ))
  }

  # Extract all values and reshape
  # For each variate, we have n_uncertainty * n_variability simulations
  result_list <- list()

  for (i in seq_len(n_variates)) {
    # Extract all simulations for this variate
    variate_values <- as.numeric(mcnode[,, i])
    n_sims <- length(variate_values)

    # Create data frame for this variate
    variate_df <- cbind(
      keys_df[rep(i, n_sims), , drop = FALSE],
      data.frame(
        variate = i,
        simulation = seq_len(n_sims),
        value = variate_values,
        stringsAsFactors = FALSE
      )
    )
    rownames(variate_df) <- NULL
    result_list[[i]] <- variate_df
  }

  # Combine all variates
  long_df <- do.call(rbind, result_list)
  long_df <- as.data.frame(long_df, stringsAsFactors = FALSE)

  return(long_df)
}


#' Plot Monte Carlo Node with Boxplot and Points
#'
#' Creates a ggplot visualization of Monte Carlo node data with semitransparent
#' boxplots overlaid with scattered points representing individual iterations.
#'
#' @param mcmodule An mcmodule object containing the node (optional if mcnode provided)
#' @param mc_name Character string specifying the name of the mcnode in the module (optional)
#' @param mcnode An mcnode object to plot directly (optional if mcmodule and mc_name provided)
#' @param data Optional data frame containing the input data
#' @param keys_names Vector of column names to use as key columns for grouping (optional).
#'   If NULL, will use node keys from the module or use row index
#' @param color_by Optional column name to color points and boxplot by (must be in keys_names or data)
#' @param order_by Optional column name or "median" to reorder y-axis groups by a value.
#'   If "median", groups will be ordered by median value
#' @param threshold Optional numeric value to add a vertical reference line
#' @param scale Optional transformation for x-axis. Use "log10" for log scale or "sqrt" for square root
#' @param max_dots Maximum number of uncertainty simulation dots to plot per variate. If exceeded,
#'   representative sampling using regular intervals is applied (default: 300). Boxplots always
#'   use all simulations for statistical accuracy.
#' @param point_alpha Transparency level for points (0-1, default: 0.4)
#' @param boxplot_alpha Transparency level for boxplot (0-1, default: 0.3)
#' @param color_pal Optional named character vector of colors for color_by categories
#'
#' @return A ggplot2 object
#'
#' @details
#' Default colors are based on the mcmodule color legend:
#' - inputs: "#B0DFF9"
#' - in_node: "#6ABDEB"
#' - out_node: "#A4CF96"
#' - trials_info: "#FAE4CB"
#' - total: "#F39200"
#' - agg_total: "#C17816"
#'
#' @examples
#' # Basic plot using mcmodule and mc_name
#' mc_plot(imports_mcmodule, "w_prev")
#'
#' # Plot with custom coloring and ordering
#' mc_plot(imports_mcmodule, "w_prev",
#'   color_by = "origin",
#'   order_by = "median"
#' )
#'
#' # Plot with threshold and scale transformation
#' mc_plot(imports_mcmodule, "no_detect_a",
#'   threshold = 0.5,
#'   scale = "log10"
#' )
#'
#' @export
mc_plot <- function(
  mcmodule = NULL,
  mc_name = NULL,
  mcnode = NULL,
  data = NULL,
  keys_names = NULL,
  color_by = NULL,
  order_by = NULL,
  threshold = NULL,
  scale = NULL,
  max_dots = 300,
  point_alpha = 0.4,
  boxplot_alpha = 0.3,
  color_pal = NULL
) {
  # Input validation
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "ggplot2 is required for mc_plot. Install it using: install.packages('ggplot2')"
    )
  }

  # Convert to long format
  long_df <- tidy_mcnode(
    mcmodule = mcmodule,
    mc_name = mc_name,
    mcnode = mcnode,
    data = data,
    keys_names = keys_names
  )

  # Y-axis will show individual variates (scenarios/rows from data)
  # Determine the grouping variable for y-axis labels
  key_cols <- setdiff(names(long_df), c("variate", "simulation", "value"))

  if (length(key_cols) > 0) {
    # Create combined label from all key columns
    if (length(key_cols) == 1) {
      long_df$y_label <- as.character(long_df[[key_cols[1]]])
    } else {
      # Combine multiple keys into a single label
      long_df$y_label <- apply(
        long_df[, key_cols, drop = FALSE],
        1,
        function(x) paste(x, collapse = " | ")
      )
    }
  } else {
    # Use row_id if no other keys
    long_df$y_label <- as.character(long_df$variate)
  }

  # Sampling: select which simulation dots to plot per variate
  n_simulations <- length(unique(long_df$simulation))
  if (n_simulations > max_dots) {
    # Use regular intervals for representative sampling
    simulation_indices <- round(seq(1, n_simulations, length.out = max_dots))
  } else {
    simulation_indices <- seq_len(n_simulations)
  }

  # Create separate data: all simulations for boxplot, sampled for points
  long_df_boxplot <- long_df
  long_df_points <- long_df[long_df$simulation %in% simulation_indices, ]

  # Handle ordering by median if requested
  if (!is.null(order_by) && order_by == "median") {
    # Order variates by median value
    median_vals <- stats::aggregate(
      long_df_boxplot$value,
      list(y_label = long_df_boxplot$y_label),
      stats::median
    )
    ordered_labels <- median_vals$y_label[order(median_vals$x)]
    long_df_boxplot$y_label <- factor(
      long_df_boxplot$y_label,
      levels = ordered_labels
    )
    long_df_points$y_label <- factor(
      long_df_points$y_label,
      levels = ordered_labels
    )
  }

  # Create base plot with flipped axes (variates on y-axis, values on x-axis)
  # Start with boxplot using ALL variates
  p <- ggplot2::ggplot(
    long_df_boxplot,
    ggplot2::aes(x = .data$value, y = .data$y_label)
  )

  # Add boxplot using all variates with optional color mapping
  if (!is.null(color_by) && color_by %in% names(long_df_boxplot)) {
    p <- p +
      ggplot2::geom_boxplot(
        ggplot2::aes(fill = .data[[color_by]]),
        alpha = boxplot_alpha,
        outlier.alpha = 0,
        color = "gray30"
      )
  } else {
    p <- p +
      ggplot2::geom_boxplot(
        alpha = boxplot_alpha,
        outlier.alpha = 0,
        color = "gray30",
        fill = "gray80"
      )
  }

  # Add sampled points with optional color mapping
  if (!is.null(color_by) && color_by %in% names(long_df_points)) {
    p <- p +
      ggplot2::geom_point(
        data = long_df_points,
        ggplot2::aes(color = .data[[color_by]]),
        alpha = point_alpha,
        position = ggplot2::position_jitter(width = 0, height = 0.15),
        size = 2
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        data = long_df_points,
        alpha = point_alpha,
        position = ggplot2::position_jitter(width = 0, height = 0.15),
        size = 2,
        color = "gray50"
      )
  }

  # Apply color palette if provided or use default
  if (!is.null(color_by) && color_by %in% names(long_df_points)) {
    if (is.null(color_pal)) {
      # Default color palette from mc_network.R
      default_pal <- c(
        inputs = "#B0DFF9",
        in_node = "#6ABDEB",
        out_node = "#A4CF96",
        trials_info = "#FAE4CB",
        total = "#F39200",
        agg_total = "#C17816"
      )
      color_pal <- default_pal
    }

    # Get unique values in color_by column
    unique_vals <- unique(long_df_points[[color_by]])

    # If color_pal has names, use named mapping; otherwise cycle through colors
    if (!is.null(names(color_pal))) {
      # Named palette - map values that exist in names
      mapped_colors <- color_pal[unique_vals]
      # For values not in palette, cycle through available colors
      unmapped_idx <- is.na(mapped_colors)
      if (any(unmapped_idx)) {
        mapped_colors[unmapped_idx] <- color_pal[seq_len(sum(unmapped_idx))]
      }
      names(mapped_colors) <- unique_vals
    } else {
      # Unnamed palette - cycle through colors
      mapped_colors <- color_pal[
        seq_along(unique_vals) %% length(color_pal) + 1
      ]
      names(mapped_colors) <- unique_vals
    }

    p <- p +
      ggplot2::scale_color_manual(values = mapped_colors, na.value = "gray50") +
      ggplot2::scale_fill_manual(values = mapped_colors, na.value = "gray80")
  }

  # Add threshold line if specified (vertical line since value is on x-axis)
  if (!is.null(threshold)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = threshold,
        linetype = "dashed",
        color = "red",
        linewidth = 0.8,
        alpha = 0.7
      )
  }

  # Apply scale transformation if specified (to x-axis since value is on x-axis)
  if (!is.null(scale)) {
    if (scale == "log10") {
      p <- p + ggplot2::scale_x_log10()
    } else if (scale == "sqrt") {
      p <- p + ggplot2::scale_x_sqrt()
    } else if (scale == "log") {
      p <- p + ggplot2::scale_x_log10()
    }
  }

  # Add labels and theme
  p <- p +
    ggplot2::labs(
      x = if (!is.null(mc_name)) mc_name else "Value",
      y = "Variate",
      title = if (!is.null(mc_name)) {
        paste("Monte Carlo Plot:", mc_name)
      } else {
        "Monte Carlo Plot"
      }
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      legend.position = "right",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}
