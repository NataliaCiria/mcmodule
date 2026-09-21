#' Create mcnodes from Data and Configuration Table
#'
#' Creates mcnodes based on mctable specifications and input data.
#' Applies transformations and generates mcnodes in the calling environment.
#'
#' Distribution functions may be supplied as bare (`function`) or namespace-qualified names (`package::function`, and `package:::function`).
#' Configured transformation expressions are evaluated as R code.
#'
#' @param data (data frame). Input data containing variables for mcnode creation.
#' @param mctable (data frame). Configuration table with columns:
#'   mcnode, mc_func, transformation, from_variable.
#' @param envir (environment, optional). Environment where nodes are created.
#'   Default: parent.frame().
#'
#' @return `NULL`, invisibly. mcnodes are created in `envir`.
#'
#' @seealso [eval_module()] for creating and evaluating a complete module and
#'   [matrix_to_mcnodes()] for creating nodes from a sampling matrix.
#'
#' @import mc2d
#' @examples
#' create_mcnodes(
#'   data = imports_data,
#'   mctable = imports_mctable
#' )
#'
#' @export
create_mcnodes <- function(
    data,
    mctable = set_mctable(),
    envir = parent.frame()
) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!is.data.frame(mctable)) {
    stop("mctable must be a data frame")
  }

  # Validate that mctable has required columns
  valid_mctable <- all(c("mcnode", "mc_func") %in% names(mctable))
  if (!valid_mctable) {
    stop("mctable must contain 'mcnode' and 'mc_func' columns")
  }

  # Validate that mctable is not empty
  if (nrow(mctable) < 1) {
    stop("mctable has 0 rows")
  }

  # Validate that data is not empty
  if (nrow(data) < 1) {
    stop("data has 0 rows")
  }

  # Check if data contains any columns matching mcnode names
  data_mc_inputs <- grepl(
    paste(paste0("\\<", mctable$mcnode, ".*"), collapse = "|"),
    names(data)
  )
  if (!any(data_mc_inputs)) {
    stop("data must contain columns matching mctable 'mcnode' names")
  }

  # Check and clean mctable
  mctable <- check_mctable(mctable)

  # Process each Monte Carlo node defined in mctable
  for (i in seq_along(mctable$mcnode)) {
    # Extract current mcnode configuration
    mcrow <- mctable[i, ]
    mc_name <- mcrow$mcnode

    #### TRANSFORM INPUT DATA IF SPECIFIED ####
    # Check if transformation is needed and source data exists
    transformation <- as.character(mcrow$transformation)
    value_transform_l <- any(
      c(mcrow$mcnode, mcrow$from_variable) %in% names(data)
    ) &
      !is.na(transformation)

    if (value_transform_l) {
      # Determine source variable and apply transformation
      value_name <- ifelse(
        is.na(mcrow$from_variable),
        as.character(mcrow$mcnode),
        as.character(mcrow$from_variable)
      )
      assign("value", data[[value_name]], envir = envir)
      data[as.character(mc_name)] <- eval(
        parse(text = transformation),
        envir = envir
      )
      rm("value", envir = envir)
    }

    # Identify columns that correspond to this Monte Carlo node
    mc_inputs_l <- grepl(paste0("\\<", mc_name, "(\\>|_[^_]*\\>)"), names(data))

    ##### PROCESS MONTE CARLO NODE INPUTS ####
    if (any(mc_inputs_l)) {
      # Get all related input columns (e.g., min, max, mode)
      mc_inputs <- names(data)[mc_inputs_l]

      # Process each input parameter
      for (j in seq_along(mc_inputs)) {
        # Validate input data type
        if (
          !is.numeric(data[[mc_inputs[j]]]) & !is.logical(data[[mc_inputs[j]]])
        ) {
          warning(paste0(
            mc_inputs[j],
            " is ",
            class(data[[mc_inputs[j]]]),
            " and should be numeric or logical"
          ))
          next
        }

        #### CREATE MONTE CARLO DATA OBJECTS ####
        input_mcnode <- mcdata(
          data = data[[mc_inputs[j]]],
          type = "0",
          nvariates = nrow(data)
        )

        # Handle NA values based on whether it's a distribution
        if (is.na(mcrow$mc_func)) {
          assign(
            mc_inputs[j],
            mcnode_na_rm(input_mcnode),
            envir = envir
          )
        } else {
          assign(mc_inputs[j], input_mcnode, envir = envir)
        }
      }

      #### CREATE DISTRIBUTION-BASED MONTE CARLO NODES ####
      if (!is.na(mcrow$mc_func)) {
        # Extract distribution function parameters
        mc_func <- as.character(mcrow$mc_func)
        mc_function <- tryCatch(
          {
            if (
              identical(mc_func, "rpert") &&
              "package:freedom" %in% search()
            ) {
              warning(
                paste0(
                  "`freedom::rpert()` is masking `mc2d::rpert()`. ",
                  "`create_mcnodes()` will use `mc2d::rpert()`. ",
                  "Use `mc2d::rpert` in `mctable$mc_func` to be explicit."
                ),
                call. = FALSE
              )
              mc2d::rpert
            } else if (identical(mc_func, "rpert")) {
              mc2d::rpert
            } else if (grepl(":::", mc_func, fixed = TRUE)) {
              function_parts <- strsplit(mc_func, ":::", fixed = TRUE)[[1]]
              getFromNamespace(function_parts[[2]], function_parts[[1]])
            } else if (grepl("::", mc_func, fixed = TRUE)) {
              function_parts <- strsplit(mc_func, "::", fixed = TRUE)[[1]]
              getExportedValue(function_parts[[1]], function_parts[[2]])
            } else {
              get(mc_func, mode = "function", inherits = TRUE)
            }
          },
          error = function(e) {
            temporary_inputs <- intersect(
              setdiff(mc_inputs, as.character(mc_name)),
              ls(envir = envir, all.names = TRUE)
            )
            if (length(temporary_inputs) > 0) {
              remove(list = temporary_inputs, envir = envir)
            }
            stop(
              sprintf(
                "Distribution function '%s' for %s could not be resolved",
                mc_func,
                mc_name
              ),
              call. = FALSE
            )
          }
        )
        func_args <- setdiff(
          names(formals(mc_function)),
          c("n", "nsv", "nsu", "nvariates", "...")
        )

        # Map parameters to input data
        mc_parameters <- paste(mc_name, func_args, sep = "_")
        parameters_available <- func_args[mc_parameters %in% mc_inputs]

        # Validate required parameters exist
        valid_parameters <- length(parameters_available) > 0
        if (!valid_parameters) {
          warning(paste0(
            mc_name,
            " ",
            mc_func,
            " mcstoc node not created because no ",
            mc_func,
            " parameter was provided. ",
            mc_name,
            " mcdata node created \n"
          ))
          temporary_inputs <- intersect(
            setdiff(mc_inputs, as.character(mc_name)),
            ls(envir = envir, all.names = TRUE)
          )
          if (length(temporary_inputs) > 0) {
            remove(list = temporary_inputs, envir = envir)
          }
          next
        }

        # Match each parameter to its corresponding column name
        matched_inputs <- sapply(parameters_available, function(param) {
          param_name <- paste(mc_name, param, sep = "_")
          matched <- mc_inputs[mc_inputs == param_name]
          if (length(matched) == 0) {
            stop(paste0(
              "Parameter '",
              param,
              "' for ",
              mc_name,
              " does not have a matching column in data"
            ))
          }
          if (length(matched) > 1) {
            warning(paste0(
              "Multiple columns match parameter '",
              param,
              "' for ",
              mc_name,
              ", using first match"
            ))
            matched <- matched[1]
          }
          matched
        })

        create_stochastic_node <- function(remove_na = FALSE) {
          parameter_nodes <- lapply(matched_inputs, function(input_name) {
            get(input_name, envir = envir, inherits = FALSE)
          })
          if (remove_na) {
            parameter_nodes <- lapply(parameter_nodes, mcnode_na_rm)
          }
          names(parameter_nodes) <- parameters_available

          do.call(
            mcstoc,
            c(
              list(func = mc_function, type = "V"),
              parameter_nodes,
              list(nvariates = nrow(data))
            )
          )
        }

        #### ATTEMPT NODE CREATION WITH ERROR HANDLING ####
        tryCatch(
          assign(
            as.character(mc_name),
            create_stochastic_node(),
            envir = envir
          ),
          error = function(e) {
            message(
              "An error occurred generating ",
              mc_name,
              " ",
              mc_func,
              " mcstoc node:\n",
              e
            )
          },
          warning = function(w) {
            #### RETRY WITH NA REMOVAL IF INITIAL ATTEMPT FAILS ####
            tryCatch(
              assign(
                as.character(mc_name),
                suppressWarnings(create_stochastic_node(remove_na = TRUE)),
                envir = envir
              ),
              error = function(e) {
                message(
                  "After mcnode_na_rm: An error occurred generating ",
                  mc_name,
                  " ",
                  mc_func,
                  " mcstoc node:\n",
                  e
                )
              },
              warning = function(w) {
                message(
                  "After mcnode_na_rm: A warning occurred generating ",
                  mc_name,
                  " ",
                  mc_func,
                  " mcstoc node:\n",
                  w,
                  "Check data inputs: is min < mode < max?\n"
                )
              }
            )
          },
          finally = {
            # Cleanup temporary input objects
            temporary_inputs <- intersect(
              setdiff(mc_inputs, as.character(mc_name)),
              ls(envir = envir, all.names = TRUE)
            )
            if (length(temporary_inputs) > 0) {
              remove(list = temporary_inputs, envir = envir)
            }
          }
        )
      }
    }
  }

  invisible(NULL)
}

#' Create mcnodes from Matrix/Data Frame
#'
#' Creates one `mcdata` mcnode per column in `X`, using `nsv = nrow(X)`.
#' This is useful when `X` is a design matrix generated by functions such as
#' [sensitivity::morris()] or [sensobol::sobol_matrices()].
#'
#' @param X (matrix or data frame). Input values with one mcnode per column.
#' @param envir (environment, optional). Environment where nodes are created.
#'   Default: parent.frame().
#'
#' @return `NULL`, invisibly. mcnodes are created in `envir`.
#'
#' @seealso [create_mcnodes()] for creating nodes from data and an `mctable`,
#'   and [eval_module()] for evaluating a complete module.
#'
#' @examples
#' X <- matrix(c(0.1, 0.2, 0.3, 10, 11, 12), ncol = 2)
#' colnames(X) <- c("a", "b")
#' matrix_to_mcnodes(X)
#'
#' @export
matrix_to_mcnodes <- function(X, envir = parent.frame()) {
  if (!(is.matrix(X) || is.data.frame(X))) {
    stop("X must be a matrix or data frame")
  }

  x_names <- colnames(X)
  X <- as.data.frame(X, stringsAsFactors = FALSE, check.names = FALSE)

  if (nrow(X) < 1) {
    stop("X has 0 rows")
  }

  if (ncol(X) < 1) {
    stop("X has 0 columns")
  }

  if (is.null(x_names)) {
    x_names <- paste0("x", seq_len(ncol(X)))
  }
  colnames(X) <- x_names

  if (anyNA(colnames(X)) || any(!nzchar(colnames(X)))) {
    stop("X column names must not be missing or empty")
  }
  if (anyDuplicated(colnames(X))) {
    stop("X column names must be unique")
  }

  for (i in seq_len(ncol(X))) {
    node_name <- colnames(X)[i]
    node_values <- X[[i]]

    if (!is.numeric(node_values) && !is.logical(node_values)) {
      stop(
        sprintf(
          "Column '%s' must be numeric or logical to create mcdata mcnode",
          node_name
        )
      )
    }

    assign(
      node_name,
      mcdata(data = node_values, type = "V", nsv = nrow(X), nvariates = 1),
      envir = envir
    )
  }

  invisible(NULL)
}
