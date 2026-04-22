#' Convert Monte Carlo Module to Matrices
#'
#' Transforms an mcmodule into a list of matrices, with one matrix per variate.
#' Each matrix has uncertainty simulations as rows and mcnodes as columns.
#'
#' @param mcmodule (mcmodule object). Module to convert.
#' @param mc_names (character vector, optional). Node names to include. If NULL,
#'   includes all nodes. Default: NULL.
#'
#' @return A list of matrices (one per variate). Each matrix has uncertainty
#'   simulations as rows and mcnodes as columns.
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

#' Set or Get Global Sample Design
#'
#' Manages a global sample design matrix/data frame by setting or retrieving it.
#' This object is typically the output of [sample_design()] and can be used as
#' default input in [eval_module()].
#'
#' @param data (matrix, data frame, or list, optional). Sample design to store
#'   globally. Accepts a matrix/data frame or a list with element `X`
#'   (typically output of [sample_design()]). If `NULL`, returns the current
#'   global sample design. Default: `NULL`.
#'
#' @return Current or newly set sample design (`list` with elements `sa` and
#'   `X`) or `NULL` if no sample design has been set.
#'
#' @examples
#' # Get current sample design (NULL if not set)
#' current_sample_design <- set_sample_design()
#'
#' # Set sample design
#' X <- data.frame(a = c(0.1, 0.2), b = c(1, 2))
#' set_sample_design(X)
#'
#' # Reset sample design
#' reset_sample_design()
#'
#' @export
set_sample_design <- function(data = NULL) {
  if (is.null(data)) {
    if (!exists("sample_design", envir = .pkgglobalenv)) {
      assign("sample_design", NULL, envir = .pkgglobalenv)
    }
    return(get("sample_design", envir = .pkgglobalenv))
  }

  sample_design_obj <- NULL
  if (is.matrix(data) || is.data.frame(data)) {
    sample_design_obj <- list(
      sa = NULL,
      X = as.data.frame(
        data,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  } else if (is.list(data)) {
    if (!"X" %in% names(data)) {
      stop("sample_design list must contain element 'X'")
    }
    if (!(is.matrix(data$X) || is.data.frame(data$X))) {
      stop("sample_design$X must be a matrix or data frame")
    }
    sample_design_obj <- list(
      sa = data$sa,
      X = as.data.frame(
        data$X,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  } else {
    stop(
      "sample_design must be a matrix, data frame, or list with element 'X'"
    )
  }

  assign("sample_design", sample_design_obj, envir = .pkgglobalenv)
  message("sample_design set to ", deparse(substitute(data)))
}

#' Reset Global Sample Design
#'
#' Clears and resets the global sample design to `NULL`.
#'
#' @return `NULL` (invisibly). Clears global sample design.
#'
#' @examples
#' reset_sample_design()
#'
#' @export
reset_sample_design <- function() {
  assign("sample_design", NULL, envir = .pkgglobalenv)
  message("sample_design reset")
}

#' Generate Sampling Design Matrix
#'
#' Builds a design object from `mctable` definitions using Morris or Sobol
#' sampling. Optionally samples only a subset of nodes via `mc_names` and
#' controls treatment of non-sampled nodes with `if_not_sampled`.
#'
#' @param mctable (data frame). Table containing at least `mcnode` and
#'   `sample_space`; may also contain `transformation`. Default: [set_mctable()].
#' @param n (integer). Number of samples for Sobol methods. Default: 1000.
#' @param method (character). Sampling method: one of `"morris"` or
#'  `"sobol"`. Default: `"morris"`.
#' @param mc_names (character vector, optional). Node names to sample. If
#'   `NULL`, all nodes in `mctable$mcnode` are sampled.
#' @param if_not_sampled (character). How to handle nodes not listed in
#'   `mc_names`: `"exclude"`, `"median"`, `"mean"`, `"max"`, or `"min"`.
#'   Default: `"exclude"`.
#' @param transformation (logical). Whether to apply `transformation` rules.
#'   Default: `TRUE`.
#' @param morris_r (integer). Number of Morris repetitions.
#'   Default: 10 (aligned with this package's default Morris setup).
#' @param morris_design (list). Morris design specification passed to
#'   [sensitivity::morris()]. Default: `list(type = "oat", levels = 4,
#'   grid.jump = 2)`, matching the defaults used by this package for Morris
#'   sampling.
#' @param sobol_scheme (character). Scheme passed to
#'   [sensitivity::sobolSalt()]. Default: `"A"`.
#' @param ... Additional arguments reserved for future extensions.
#'
#' @return A sensitivity object directly:
#'   \itemize{
#'     \item For `method = "morris"`: an object of class `"morris"`
#'       returned by [sensitivity::morris()].
#'     \item For `method = "sobol"`: an object of class `"sobolSalt"`
#'       returned by [sensitivity::sobolSalt()].
#'   }
#'   The sampled design matrix can be accessed from object components
#'   (`$X` for Morris; `$X1`, `$X2`, and `$X` for Sobol).
#'
#' @examples
#' mctable <- data.frame(
#'   mcnode = c("x", "y"),
#'   sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
#'   stringsAsFactors = FALSE
#' )
#' sd_morris <- sample_design(mctable, n = 10, method = "morris")
#' head(sd_morris$X)
#'
#' sd_sobol <- sample_design(mctable, n = 16, method = "sobol")
#' head(sd_sobol$X1)
sample_design <- function(
  mctable = set_mctable(),
  n = 1000,
  method = c("morris", "sobol"),
  mc_names = NULL,
  if_not_sampled = c("exclude", "median", "mean", "max", "min"),
  transformation = TRUE,
  morris_r = 10,
  morris_design = list(type = "oat", levels = 4, grid.jump = 2),
  sobol_scheme = "A",
  ...
) {
  method <- match.arg(method)
  if_not_sampled <- match.arg(if_not_sampled)
  sa <- NULL

  # Filter mctable by mc_names if provided
  all_mcnode_names <- mctable$mcnode
  if (!is.null(mc_names)) {
    mc_names <- as.character(mc_names)
    invalid_names <- setdiff(mc_names, all_mcnode_names)
    if (length(invalid_names) > 0) {
      stop(sprintf(
        "Invalid mc_names: %s not in mctable$mcnode",
        paste(invalid_names, collapse = ", ")
      ))
    }
    mctable_sampled <- mctable[mctable$mcnode %in% mc_names, ]
    mctable_not_sampled <- mctable[!(mctable$mcnode %in% mc_names), ]
  } else {
    mc_names <- all_mcnode_names
    mctable_sampled <- mctable
    mctable_not_sampled <- mctable[FALSE, ] # Empty data frame with same structure
  }

  # Treat NA/empty sample_space as non-sampled inputs
  sampled_ss <- trimws(as.character(mctable_sampled$sample_space))
  na_sample_space_idx <- is.na(mctable_sampled$sample_space) | sampled_ss == ""
  if (any(na_sample_space_idx)) {
    moved_to_not_sampled <- mctable_sampled[na_sample_space_idx, , drop = FALSE]
    mctable_sampled <- mctable_sampled[!na_sample_space_idx, , drop = FALSE]
    mctable_not_sampled <- rbind(mctable_not_sampled, moved_to_not_sampled)
  }

  input_names <- mctable_sampled$mcnode
  sample_space <- mctable_sampled$sample_space

  parse_sample_space <- function(ss) {
    ss <- trimws(as.character(ss))
    if (is.na(ss) || ss == "") {
      stop("sample_space must not be NA or empty")
    }

    if (grepl("^c\\s*\\(", ss)) {
      vals <- eval(parse(text = ss), envir = baseenv())
      return(list(kind = "vector", values = vals))
    }

    if (grepl("=", ss)) {
      parts <- unlist(strsplit(ss, ",\\s*"))
      keys <- trimws(sub("=.*$", "", parts))
      vals_chr <- trimws(sub("^[^=]*=", "", parts))
      vals <- lapply(vals_chr, function(x) {
        if (x %in% c("TRUE", "FALSE")) {
          return(as.logical(x))
        }
        if (grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", x)) {
          return(as.numeric(x))
        }
        gsub("^['\"]|['\"]$", "", x)
      })
      names(vals) <- keys
      return(list(kind = "named", values = vals))
    }

    stop("Unsupported sample_space format")
  }

  sample_from_space <- function(ss, n) {
    parsed <- parse_sample_space(ss)

    if (identical(parsed$kind, "vector")) {
      vals <- parsed$values
      if (length(vals) == 0) {
        stop("sample_space vector cannot be empty")
      }
      if (is.numeric(vals) && length(vals) == 2) {
        return(runif(n, min = vals[1], max = vals[2]))
      }
      return(sample(vals, size = n, replace = TRUE))
    }

    vals <- parsed$values
    vals_un <- unlist(vals, use.names = FALSE)
    if (
      all(vapply(vals, is.numeric, logical(1))) &&
        all(c("min", "max") %in% names(vals))
    ) {
      return(runif(n, min = vals$min, max = vals$max))
    }

    if (all(vapply(vals, is.logical, logical(1)))) {
      return(sample(as.logical(vals_un), size = n, replace = TRUE))
    }

    if (
      all(vapply(
        vals,
        function(x) is.character(x) && length(x) == 1,
        logical(1)
      ))
    ) {
      return(sample(as.character(vals_un), size = n, replace = TRUE))
    }

    if (all(vapply(vals, is.numeric, logical(1))) && length(vals_un) == 1) {
      return(rep(as.numeric(vals_un), n))
    }

    sample(vals_un, size = n, replace = TRUE)
  }

  # Apply transformations upfront (before sampling) for Morris/Sobol
  # This ensures transformed values' bounds are used for numeric extraction
  if (isTRUE(transformation) && method %in% c("morris", "sobol")) {
    transformations <- if ("transformation" %in% names(mctable)) {
      mctable$transformation
    } else {
      rep(NA_character_, length(input_names))
    }

    # For each node, sample from original space, apply transformation,
    # extract new bounds, and update sample_space
    for (i in seq_along(input_names)) {
      transform_i <- transformations[i]
      if (!is.na(transform_i) && nzchar(trimws(transform_i))) {
        # Sample probe values from original space to determine transformed bounds
        ss_orig <- sample_space[i]
        probe_vals <- sample_from_space(ss_orig, 1000)

        # Apply transformation
        transformed_vals <- eval(
          parse(text = transform_i),
          envir = list2env(list(value = probe_vals), parent = baseenv())
        )

        # Coerce logicals to numeric
        if (is.logical(transformed_vals)) {
          transformed_vals <- as.numeric(transformed_vals)
        }

        # Extract new bounds and update sample_space
        new_min <- min(transformed_vals, na.rm = TRUE)
        new_max <- max(transformed_vals, na.rm = TRUE)
        sample_space[i] <- sprintf("min = %g, max = %g", new_min, new_max)
      }
    }
  }

  parse_sample_space <- function(ss) {
    ss <- trimws(as.character(ss))
    if (is.na(ss) || ss == "") {
      stop("sample_space must not be NA or empty")
    }

    if (grepl("^c\\s*\\(", ss)) {
      vals <- as.numeric(eval(parse(text = ss), envir = baseenv()))
      return(list(kind = "vector", values = vals))
    }

    if (grepl("=", ss)) {
      parts <- unlist(strsplit(ss, ",\\s*"))
      keys <- trimws(sub("=.*$", "", parts))
      vals_chr <- trimws(sub("^[^=]*=", "", parts))
      vals <- lapply(vals_chr, function(x) {
        if (x %in% c("TRUE", "FALSE")) {
          return(as.numeric(x))
        }
        if (grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", x)) {
          return(as.numeric(x))
        }
        gsub("^['\"]|['\"]$", "", x)
      })
      names(vals) <- keys
      return(list(kind = "named", values = vals))
    }

    stop("Unsupported sample_space format")
  }

  sample_from_space <- function(ss, n) {
    parsed <- parse_sample_space(ss)

    if (identical(parsed$kind, "vector")) {
      vals <- parsed$values
      if (length(vals) == 0) {
        stop("sample_space vector cannot be empty")
      }
      if (is.numeric(vals) && length(vals) == 2) {
        return(runif(n, min = vals[1], max = vals[2]))
      }
      return(sample(vals, size = n, replace = TRUE))
    }

    vals <- parsed$values
    vals_un <- unlist(vals, use.names = FALSE)
    if (
      all(vapply(vals, is.numeric, logical(1))) &&
        all(c("min", "max") %in% names(vals))
    ) {
      return(runif(n, min = vals$min, max = vals$max))
    }

    if (all(vapply(vals, is.logical, logical(1)))) {
      return(sample(as.logical(vals_un), size = n, replace = TRUE))
    }

    if (
      all(vapply(
        vals,
        function(x) is.character(x) && length(x) == 1,
        logical(1)
      ))
    ) {
      return(sample(as.character(vals_un), size = n, replace = TRUE))
    }

    if (all(vapply(vals, is.numeric, logical(1))) && length(vals_un) == 1) {
      return(rep(as.numeric(vals_un), n))
    }

    sample(vals_un, size = n, replace = TRUE)
  }

  extract_numeric_bounds <- function() {
    binf <- numeric(length(input_names))
    bsup <- numeric(length(input_names))
    for (i in seq_along(input_names)) {
      ss <- sample_space[i]
      parsed <- parse_sample_space(ss)

      if (identical(parsed$kind, "named")) {
        vals <- parsed$values
        if (
          all(c("min", "max") %in% names(vals)) &&
            all(vapply(vals, is.numeric, logical(1)))
        ) {
          binf[i] <- as.numeric(vals$min)
          bsup[i] <- as.numeric(vals$max)
        } else {
          stop(
            sprintf(
              "Cannot extract numeric bounds from sample_space for '%s'. Use format 'min = X, max = Y' for Morris/Sobol.",
              input_names[i]
            )
          )
        }
      } else if (identical(parsed$kind, "vector")) {
        vals <- parsed$values
        if (is.numeric(vals) && length(vals) == 2) {
          binf[i] <- vals[1]
          bsup[i] <- vals[2]
        } else {
          stop(
            sprintf(
              "Cannot extract numeric bounds from sample_space for '%s'. Use format 'c(min, max)' or 'min = X, max = Y' for Morris/Sobol.",
              input_names[i]
            )
          )
        }
      } else {
        stop(
          sprintf(
            "sample_space for '%s' does not support Morris/Sobol sampling.",
            input_names[i]
          )
        )
      }
    }
    list(binf = binf, bsup = bsup)
  }

  # Route by method
  if (method == "morris") {
    # For Morris, only sample the factors in input_names
    # Non-sampled factors (if any) will be added later with fixed values
    bounds <- extract_numeric_bounds()
    if (!requireNamespace("sensitivity", quietly = TRUE)) {
      stop(
        "Package 'sensitivity' required for method='morris'. Install via install.packages('sensitivity')."
      )
    }

    morris_res <- sensitivity::morris(
      model = NULL,
      factors = input_names,
      r = morris_r,
      design = morris_design,
      binf = bounds$binf,
      bsup = bounds$bsup,
      scale = TRUE
    )

    X <- as.data.frame(
      morris_res$X,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    colnames(X) <- input_names
    sa <- morris_res
  } else if (method == "sobol") {
    # For Sobol, only sample the factors in input_names
    bounds <- extract_numeric_bounds()
    if (!requireNamespace("sensitivity", quietly = TRUE)) {
      stop(
        "Package 'sensitivity' required for method='sobol'. Install via install.packages('sensitivity')."
      )
    }

    X1 <- matrix(NA_real_, nrow = n, ncol = length(input_names))
    X2 <- matrix(NA_real_, nrow = n, ncol = length(input_names))

    for (j in seq_along(input_names)) {
      X1[, j] <- runif(n, min = bounds$binf[j], max = bounds$bsup[j])
      X2[, j] <- runif(n, min = bounds$binf[j], max = bounds$bsup[j])
    }

    colnames(X1) <- input_names
    colnames(X2) <- input_names

    sobol_res <- sensitivity::sobolSalt(
      model = NULL,
      X1 = X1,
      X2 = X2,
      scheme = sobol_scheme,
      nboot = 0
    )

    colnames(sobol_res$X1) <- input_names
    colnames(sobol_res$X2) <- input_names
    colnames(sobol_res$X) <- input_names

    sa <- sobol_res
  }

  # Handle non-sampled nodes based on if_not_sampled argument
  # Compute fixed values directly from sample_space without probe sampling
  fixed_factors_info <- NULL
  if (nrow(mctable_not_sampled) > 0 && if_not_sampled != "exclude") {
    fixed_factors_list <- list()

    for (i in seq_len(nrow(mctable_not_sampled))) {
      node_name <- mctable_not_sampled$mcnode[i]
      ss <- mctable_not_sampled$sample_space[i]

      # Nodes without sample_space default to 0 when held constant
      if (is.na(ss) || !nzchar(trimws(as.character(ss)))) {
        fixed_val <- 0
      } else {
        # Parse the sample space to extract bounds/values
        parsed_ss <- parse_sample_space(ss)

        # Compute fixed value directly from parsed sample space
        if (identical(parsed_ss$kind, "named")) {
          vals <- parsed_ss$values
          # For named values like "min=X, max=Y"
          if (all(c("min", "max") %in% names(vals))) {
            min_val <- as.numeric(vals$min)
            max_val <- as.numeric(vals$max)
            fixed_val <- switch(
              if_not_sampled,
              median = (min_val + max_val) / 2,
              mean = (min_val + max_val) / 2,
              max = max_val,
              min = min_val
            )
          } else {
            # For other named values, use first value
            fixed_val <- as.numeric(vals[[1]])
          }
        } else if (identical(parsed_ss$kind, "vector")) {
          vals <- parsed_ss$values
          # For vector like c(min, max) or categorical
          if (is.numeric(vals) && length(vals) == 2) {
            fixed_val <- switch(
              if_not_sampled,
              median = (vals[1] + vals[2]) / 2,
              mean = (vals[1] + vals[2]) / 2,
              max = vals[2],
              min = vals[1]
            )
          } else if (is.numeric(vals)) {
            # Numeric vector: compute statistic
            fixed_val <- switch(
              if_not_sampled,
              median = median(vals, na.rm = TRUE),
              mean = mean(vals, na.rm = TRUE),
              max = max(vals, na.rm = TRUE),
              min = min(vals, na.rm = TRUE)
            )
          } else {
            # Categorical vector: use transformation if available
            transform_val <- if (
              "transformation" %in% names(mctable_not_sampled)
            ) {
              mctable_not_sampled$transformation[i]
            } else {
              NA_character_
            }

            if (
              !is.na(transform_val) &&
                nzchar(trimws(as.character(transform_val)))
            ) {
              # Apply transformation to first value
              transformed <- eval(
                parse(text = trimws(as.character(transform_val))),
                envir = list2env(list(value = vals[1]), parent = baseenv())
              )
              fixed_val <- as.numeric(transformed)
            } else {
              fixed_val <- NA_real_
            }
          }
        } else {
          fixed_val <- NA_real_
        }
      }

      # Store with "fix." prefix
      fixed_col_name <- paste0("fix.", node_name)
      fixed_factors_list[[fixed_col_name]] <- rep(fixed_val, nrow(X))
    }

    # Add fixed factor columns to X
    for (col_name in names(fixed_factors_list)) {
      X[[col_name]] <- fixed_factors_list[[col_name]]
    }

    # Reorder columns: sampled first (in original order), then fixed factors
    sampled_cols <- intersect(all_mcnode_names, colnames(X))
    fixed_cols <- grep("^fix\\.", colnames(X), value = TRUE)
    X <- X[, c(sampled_cols, fixed_cols)]

    # Create notification message
    fixed_factors_info <- sprintf(
      "%d factor(s) held constant at %s value(s): %s",
      length(fixed_cols),
      if_not_sampled,
      paste(gsub("^fix\\.", "", fixed_cols), collapse = ", ")
    )
  }

  # Print notification if fixed factors were used
  if (!is.null(fixed_factors_info) && method %in% c("morris", "sobol")) {
    message(
      "sample_design: ",
      method,
      " sampling with fixed factors (prefix 'fix.')\n  ",
      fixed_factors_info
    )
  }

  return(sa)
}
