#' Set or Get Global Sample Design
#'
#' Manages a global sample design matrix/data frame by setting or retrieving it.
#' This object is typically a matrix with one column per input parameter and one row per sample, or
#' the output of [sensitivity::sensitivity] functions. It can be used as
#' default input in [eval_module()].
#'
#' @param data (matrix, data frame, or list, optional). Sample design to store
#'   globally. Accepts a matrix/data frame or a list with a matrix in element `X`
#'   (typically output of [sensitivity::sensitivity] functions). If `NULL`, returns the current
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

  if (is.matrix(data) || is.data.frame(data)) {
    sample_design_obj <- list(
      sa = NULL,
      X = as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
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
      X = as.data.frame(data$X, stringsAsFactors = FALSE, check.names = FALSE)
    )
  } else {
    stop("sample_design must be a matrix, data frame, or list with element 'X'")
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

# Parse sample_space strings used in mctable.
#
# Supported formats:
# - "c(...)"            (vector; numeric length-2 is treated as bounds)
# - "key = val, ..."    (named list; numeric values parsed where possible)
#
# Returns a list with:
# - kind: "vector" | "named"
# - values: vector or named list
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

# Parse numeric min/max bounds from a sample_space string.
# Returns c(min = ..., max = ...) or NULL if bounds are unavailable.
parse_sample_space_bounds <- function(ss) {
  ss <- trimws(as.character(ss))
  if (is.na(ss) || ss %in% c("", "NA")) {
    return(NULL)
  }

  parsed <- parse_sample_space(ss)

  if (identical(parsed$kind, "vector")) {
    vals <- parsed$values
    if (is.numeric(vals) && length(vals) == 2) {
      return(c(min = vals[1], max = vals[2]))
    }
    return(NULL)
  }

  vals <- parsed$values
  if (
    all(c("min", "max") %in% names(vals)) &&
      all(vapply(vals[c("min", "max")], is.numeric, logical(1)))
  ) {
    return(c(min = as.numeric(vals$min), max = as.numeric(vals$max)))
  }

  NULL
}

# Extract numeric bounds, with informative errors.
extract_numeric_bounds <- function(ss, node_name) {
  bounds <- parse_sample_space_bounds(ss)
  if (!is.null(bounds)) {
    return(bounds)
  }

  parsed <- parse_sample_space(ss)
  if (identical(parsed$kind, "vector")) {
    stop(sprintf(
      "Cannot extract numeric bounds from sample_space for '%s'. Use format 'c(min, max)' or 'min = X, max = Y' for Morris.",
      node_name
    ))
  }

  stop(sprintf(
    "Cannot extract numeric bounds from sample_space for '%s'. Use format 'min = X, max = Y' for Morris.",
    node_name
  ))
}

# Normalise mc_func names: "mc2d::rpert" / "mc2d:::rpert" -> "rpert".
# Returns NA_character_ for missing or empty values.
normalise_mc_func <- function(mc_func) {
  f <- trimws(as.character(mc_func))
  if (length(f) == 0 || is.na(f[[1]]) || !nzchar(f[[1]])) {
    return(NA_character_)
  }
  sub("^.*::", "", f[[1]])
}

# Quantile mapping: map probabilities `u` in [0, 1] to values of a
# sample_space definition, using the distribution in `mc_func` when available.
#
# Shared by mctable_sobol_matrices() (mapping), sample_from_space() (probing
# transformations) and is_constant_space() (constant detection).
#
# Supported:
# - mc_func "rnorm" with "mean = X, sd = Y" (u clamped to avoid +/-Inf)
# - mc_func "rpert" with "min = X, mode = Y, max = Z" (optional shape)
# - numeric bounds ("min = X, max = Y" or "c(min, max)"): uniform. Also used
#   as fallback when rnorm/rpert parameters are incomplete.
# - a single numeric value (e.g. "value = 5"): constant
# - categorical values (no mc_func): u is mapped to an index
qsample_space <- function(u, ss, mc_func = NA, node_name = "") {
  func <- normalise_mc_func(mc_func)
  if (!is.na(func) && !func %in% c("runif", "rnorm", "rpert")) {
    stop(sprintf("Unsupported mc_func '%s' for '%s'", func, node_name))
  }

  parsed <- parse_sample_space(ss)
  vals <- parsed$values
  bounds <- parse_sample_space_bounds(ss)

  has_params <- function(keys) {
    identical(parsed$kind, "named") &&
      all(keys %in% names(vals)) &&
      all(vapply(vals[keys], is.numeric, logical(1)))
  }

  if (identical(func, "rnorm") && has_params(c("mean", "sd"))) {
    u <- pmin(1 - 1e-12, pmax(1e-12, u))
    return(stats::qnorm(u, mean = vals$mean, sd = vals$sd))
  }

  if (identical(func, "rpert") && has_params(c("min", "mode", "max"))) {
    shape <- if (has_params("shape")) vals$shape else 4
    return(mc2d::qpert(
      u,
      min = vals$min,
      mode = vals$mode,
      max = vals$max,
      shape = shape
    ))
  }

  # Uniform (also fallback when rnorm/rpert parameters are incomplete)
  if (!is.null(bounds)) {
    return(bounds[["min"]] + (bounds[["max"]] - bounds[["min"]]) * u)
  }

  if (identical(func, "runif")) {
    stop(sprintf(
      "Missing numeric bounds for '%s' (runif requires min/max)",
      node_name
    ))
  }
  if (identical(func, "rnorm")) {
    stop(sprintf(
      "sample_space for '%s' must provide mean and sd for rnorm",
      node_name
    ))
  }
  if (identical(func, "rpert")) {
    stop(sprintf(
      "sample_space for '%s' must provide min, mode, and max for rpert",
      node_name
    ))
  }

  # Single numeric value, e.g. "value = 5"
  if (
    identical(parsed$kind, "named") &&
      length(vals) == 1 &&
      is.numeric(vals[[1]])
  ) {
    return(rep(as.numeric(vals[[1]]), length(u)))
  }

  # Categorical values: map u to an index
  cat_vals <- unlist(vals, use.names = FALSE)
  is_categorical <- identical(parsed$kind, "vector") ||
    !all(vapply(vals, is.numeric, logical(1)))
  if (is_categorical) {
    if (length(cat_vals) == 0) {
      stop("sample_space vector cannot be empty")
    }
    idx <- pmin(length(cat_vals), pmax(1L, ceiling(u * length(cat_vals))))
    return(cat_vals[idx])
  }

  stop(sprintf(
    paste0(
      "Cannot map sample_space for '%s': provide min/max bounds, ",
      "or set mc_func to 'rnorm' (mean, sd) or 'rpert' (min, mode, max)"
    ),
    node_name
  ))
}

# Probe values from a sample_space definition (used for probing transformations
# and detecting constant inputs).
#
# Uses a deterministic, evenly spaced grid of probabilities mapped through
# qsample_space(), so results are reproducible, include the exact endpoints of
# bounded distributions, and do not consume the random number stream.
sample_from_space <- function(ss, n, mc_func = NA, node_name = "") {
  u <- seq(0, 1, length.out = max(2L, as.integer(n)))
  qsample_space(u, ss, mc_func = mc_func, node_name = node_name)
}

# TRUE if a node takes a single value (after transformation).
is_constant_space <- function(
  ss,
  mc_func = NA,
  transform = NA,
  node_name = "",
  n_probe = 101
) {
  x <- transform_sample_values(
    sample_from_space(ss, n_probe, mc_func = mc_func, node_name = node_name),
    transform,
    node_name = node_name
  )
  length(unique(x[!is.na(x)])) <= 1
}

# Compute a fixed value from numeric bounds.
fixed_from_bounds <- function(bounds, if_not_sampled) {
  if (is.null(bounds)) {
    return(0)
  }
  switch(
    if_not_sampled,
    median = mean(bounds),
    mean = mean(bounds),
    max = bounds[["max"]],
    min = bounds[["min"]]
  )
}

# Apply an mctable transformation expression to `value`.
# Returns `value` unchanged if the transformation is missing or empty.
apply_value_transformation <- function(value, transform) {
  transform <- as.character(transform)
  if (
    length(transform) == 0 ||
    is.na(transform[[1]]) ||
    !nzchar(trimws(transform[[1]]))
  ) {
    return(value)
  }
  out <- eval(
    parse(text = trimws(transform[[1]])),
    envir = list2env(list(value = value), parent = baseenv())
  )
  as.numeric(out)
}

# Apply an mctable transformation to values drawn from a sample_space.
# If the transformation returns only NA for non-NA inputs (e.g. a categorical
# mapping such as ifelse(value == 'always', 1, ...) applied to a numeric
# sample_space like "min = 0, max = 1"), the sample_space is assumed to be
# already on the model scale and the values are returned unchanged.
transform_sample_values <- function(values, transform, node_name = "") {
  out <- suppressWarnings(apply_value_transformation(values, transform))
  if (length(out) > 0 && all(is.na(out)) && !all(is.na(values))) {
    message(sprintf(
      paste0(
        "Transformation for '%s' returns NA for all sample_space values; ",
        "assuming sample_space is already on the model scale and skipping it"
      ),
      node_name
    ))
    return(values)
  }
  out
}

# Fixed value for a non-sampled node, computed from its mctable sample_space
# bounds (if_not_sampled statistic) and, optionally, its transformation.
# Used by eval_module() and trial_totals() when a required input is missing
# from the sample design.
fixed_value_for_node <- function(
  mctable,
  mc_name,
  if_not_sampled = "median",
  transformation = TRUE
) {
  row <- if (!is.null(mctable) && "mcnode" %in% names(mctable)) {
    mctable[mctable$mcnode %in% mc_name, , drop = FALSE]
  } else {
    NULL
  }

  if (is.null(row) || nrow(row) == 0) {
    stop(sprintf(
      "Input '%s' is missing from sample_design and not found in mctable",
      mc_name
    ))
  }
  row <- row[1, , drop = FALSE]

  ss <- if ("sample_space" %in% names(row)) {
    as.character(row$sample_space)
  } else {
    NA_character_
  }
  bounds <- parse_sample_space_bounds(ss)
  if (is.null(bounds)) {
    stop(sprintf(
      "Input '%s' is missing from sample_design and has no numeric bounds in mctable$sample_space",
      mc_name
    ))
  }

  val <- fixed_from_bounds(bounds, if_not_sampled)

  if (isTRUE(transformation) && "transformation" %in% names(row)) {
    val <- transform_sample_values(
      val,
      row$transformation,
      node_name = mc_name
    )
  }

  val
}

# Filter an mctable by mc_names, and move missing/empty sample_space to "out".
split_mctable_for_sampling <- function(mctable, mc_names = NULL) {
  all_names <- as.character(mctable$mcnode)

  if (!is.null(mc_names)) {
    mc_names <- as.character(mc_names)
    invalid <- setdiff(mc_names, all_names)
    if (length(invalid) > 0) {
      stop(sprintf(
        "Invalid mc_names: %s not in mctable$mcnode",
        paste(invalid, collapse = ", ")
      ))
    }
    mctable_in <- mctable[mctable$mcnode %in% mc_names, , drop = FALSE]
    mctable_out <- mctable[!(mctable$mcnode %in% mc_names), , drop = FALSE]
  } else {
    mctable_in <- mctable
    mctable_out <- mctable[FALSE, , drop = FALSE]
  }

  ss_trim <- trimws(as.character(mctable_in$sample_space))
  moved <- is.na(mctable_in$sample_space) | ss_trim %in% c("", "NA")
  if (any(moved)) {
    mctable_out <- rbind(mctable_out, mctable_in[moved, , drop = FALSE])
    mctable_in <- mctable_in[!moved, , drop = FALSE]
  }

  list(mctable_in = mctable_in, mctable_out = mctable_out)
}

#' Extract Sample Space Bounds From mctable
#'
#' Extract the bounds required by [sensitivity::morris()] and other sampling
#' design functions from an `mctable`.
#'
#' Supports sampling only a subset of nodes via `mc_names` and controls how
#' non-sampled nodes are handled via `if_not_sampled`. If `transformation` is
#' enabled and `mctable` includes a `transformation` column, the function
#' computes bounds on the transformed values.
#'
#' @param mctable (data frame). Table containing at least `mcnode` and
#' `sample_space`; may also contain `transformation` and `mc_func` (used to
#' probe transformations). Default: [set_mctable()].
#' @param mc_names (character vector, optional). Node names to include. If
#' `NULL`, all nodes in `mctable$mcnode` are used.
#' @param if_not_sampled (character). How to handle nodes not listed in
#' `mc_names` (and nodes with missing or empty `sample_space`):
#' `"exclude"`, `"median"`, `"mean"`, `"max"`, or `"min"`.
#' Default: `"exclude"`.
#' @param transformation (logical). Whether to apply `transformation` rules.
#' Default: `TRUE`.
#' @param n_probe (integer). Number of grid points used to probe the
#' transformed range when `transformation = TRUE`. Probes are an evenly spaced
#' grid of probabilities in \[0, 1\] mapped through the distribution defined by
#' `sample_space` and `mc_func` (uniform, `rnorm`, `rpert` or categorical), so
#' results are reproducible and do not affect the random number stream.
#' Default: 1000.
#' @param drop_constant (logical). If `TRUE`, drop factors whose lower and
#' upper bounds are equal (after any transformation). Dropped factors are listed in `dropped` and, when
#' `if_not_sampled != "exclude"`, added to `fixed` with their constant value.
#' Dropped inputs are created as fixed nodes by [eval_module()] and
#' [trial_totals()] when missing from `sample_design`. Default: `TRUE`.
#'
#' @return A list `bounds` with:
#' \itemize{
#' \item `binf` numeric vector of lower bounds (same order as `factors`).
#' \item `bsup` numeric vector of upper bounds (same order as `factors`).
#' \item `factors` character vector of factor names.
#' \item `fixed` named numeric vector with fixed values for non-sampled
#' factors when `if_not_sampled != "exclude"`.
#' \item `dropped` character vector of factors removed by `drop_constant`.
#' }
#'
#' @export
mctable_bounds <- function(
    mctable = set_mctable(),
    mc_names = NULL,
    if_not_sampled = c("exclude", "median", "mean", "max", "min"),
    transformation = TRUE,
    n_probe = 1000,
    drop_constant = TRUE
) {
  if_not_sampled <- match.arg(if_not_sampled)

  if (!all(c("mcnode", "sample_space") %in% names(mctable))) {
    stop("mctable must contain columns 'mcnode' and 'sample_space'")
  }

  spl <- split_mctable_for_sampling(mctable, mc_names = mc_names)
  mctable_in <- spl$mctable_in
  mctable_out <- spl$mctable_out

  factors <- as.character(mctable_in$mcnode)
  sample_space <- as.character(mctable_in$sample_space)

  use_transformation <- isTRUE(transformation) &&
    "transformation" %in% names(mctable)

  if (!"mc_func" %in% names(mctable_in) && "func" %in% names(mctable_in)) {
    mctable_in$mc_func <- mctable_in$func
  }
  mc_func_in <- if ("mc_func" %in% names(mctable_in)) {
    as.character(mctable_in$mc_func)
  } else {
    rep(NA_character_, length(factors))
  }

  # Apply transformations by probing and updating bounds on the transformed scale.
  if (use_transformation) {
    transformations <- as.character(mctable_in$transformation)

    for (i in seq_along(factors)) {
      transform_i <- transformations[i]
      if (!is.na(transform_i) && nzchar(trimws(transform_i))) {
        probe_vals <- sample_from_space(
          sample_space[i],
          n_probe,
          mc_func = mc_func_in[i],
          node_name = factors[i]
        )
        transformed_vals <- transform_sample_values(
          probe_vals,
          transform_i,
          node_name = factors[i]
        )

        # %.15g (not %g) to avoid rounding distinct bounds to the same value
        sample_space[i] <- sprintf(
          "min = %.15g, max = %.15g",
          min(transformed_vals, na.rm = TRUE),
          max(transformed_vals, na.rm = TRUE)
        )
      }
    }
  }

  binf <- numeric(length(factors))
  bsup <- numeric(length(factors))
  for (i in seq_along(factors)) {
    b <- extract_numeric_bounds(sample_space[i], factors[i])
    binf[i] <- b[["min"]]
    bsup[i] <- b[["max"]]
  }

  # Drop factors with no variation (binf == bsup)
  dropped <- character(0)
  dropped_vals <- numeric(0)
  if (isTRUE(drop_constant) && length(factors) > 0) {
    is_constant <- !is.na(binf) & !is.na(bsup) & binf == bsup
    if (any(is_constant)) {
      dropped <- factors[is_constant]
      dropped_vals <- stats::setNames(binf[is_constant], dropped)
      factors <- factors[!is_constant]
      binf <- binf[!is_constant]
      bsup <- bsup[!is_constant]

      message(sprintf(
        "Dropped %d input(s) with no variation (binf == bsup): %s",
        length(dropped),
        paste(dropped, collapse = ", ")
      ))

      if (length(factors) == 0) {
        warning(
          "All sampled inputs were dropped by drop_constant; no factors remain",
          call. = FALSE
        )
      }
    }
  }

  fixed <- numeric(0)
  if (nrow(mctable_out) > 0 && if_not_sampled != "exclude") {
    for (i in seq_len(nrow(mctable_out))) {
      node_name <- as.character(mctable_out$mcnode[i])
      bounds_i <- parse_sample_space_bounds(mctable_out$sample_space[i])
      fixed_val <- fixed_from_bounds(bounds_i, if_not_sampled)

      if (use_transformation) {
        fixed_val <- transform_sample_values(
          fixed_val,
          mctable_out$transformation[i],
          node_name = node_name
        )
      }

      fixed[[node_name]] <- fixed_val
    }
  }

  # Constant factors are already on the transformed scale
  if (length(dropped_vals) > 0 && if_not_sampled != "exclude") {
    fixed <- c(fixed, dropped_vals)
  }

  list(
    binf = binf,
    bsup = bsup,
    factors = factors,
    fixed = fixed,
    dropped = dropped
  )
}

#' Sobol sampling matrices from an mctable
#'
#' Create Sobol sampling matrices using [sensobol::sobol_matrices()] and an
#' `mctable` definition. The function generates quasi-random draws in \[0, 1\]
#' and then maps them to the target distributions defined in `mctable$mc_func`
#' (or `mctable$func`) and `mctable$sample_space`.
#'
#' If the distribution function is missing but numeric bounds are available in
#' `sample_space` (e.g. `min = 0, max = 1` or `c(0, 1)`), the function assumes a
#' uniform distribution (`stats::runif`). Supported distribution functions are
#' `runif`, `rnorm` (`mean`, `sd`) and `rpert` (`min`, `mode`, `max`, optional
#' `shape`), also when namespace-qualified (e.g. `mc2d::rpert`). Categorical
#' `sample_space` vectors (e.g. `c('always', 'sometimes', 'never')`) are
#' supported when a numeric `transformation` is provided.
#'
#' @param mctable (data frame). Table containing at least `mcnode` and
#'   `sample_space`; may also contain `mc_func` / `func` and `transformation`.
#' @param N (integer). Base sample size (see [sensobol::sobol_matrices()]).
#' @param matrices (character). Which Sobol matrices to create (see
#'   [sensobol::sobol_matrices()]). Default: `c("A", "B", "AB")`.
#' @param order (character). Either `"first"`,  `"second"`, `"third"`, or `"fourth"` (see
#'   [sensobol::sobol_matrices()]).
#' @param type (character). Sampling design used by `sensobol::sobol_matrices()`.
#'   In sensobol 1.1.6, options include `"QRN"` (default), `"LHS"`, and `"R"`.
#' @param mc_names (character vector, optional). Node names to include. If
#'   `NULL`, all nodes in `mctable$mcnode` are used.
#' @param transformation (logical). Whether to apply `mctable$transformation`
#'   rules after mapping, so values are on the same scale as the bounds
#'   returned by [mctable_bounds()]. Required for categorical `sample_space`.
#'   Default: `TRUE`.
#' @param drop_constant (logical). If `TRUE`, exclude nodes that take a single
#'   value before building the matrices. Dropped node names are stored in
#'   `attr(X, "dropped")`. Default: `TRUE`.
#' @param ... Additional arguments passed to [sensobol::sobol_matrices()] (and
#'   potentially to `randtoolbox::sobol()` when `type = "QRN"`).
#'
#' @return A numeric matrix where each column is a model input **after mapping
#'   to the distributions defined in the `mctable`** (and transformation, if
#'   applied), and each row is a sampling point. The matrix has the same
#'   layout/row binding as [sensobol::sobol_matrices()]. Attribute `"dropped"`
#'   contains the names of nodes removed by `drop_constant`.
#'
#' @export
mctable_sobol_matrices <- function(
  mctable = set_mctable(),
  N,
  matrices = c("A", "B", "AB"),
  order = c("first", "second", "third", "fourth"),
  type = c("QRN", "LHS", "R"),
  mc_names = NULL,
  transformation = TRUE,
  drop_constant = TRUE,
  ...
) {
  if (!requireNamespace("sensobol", quietly = TRUE)) {
    stop(
      "This function needs the 'sensobol' package.\n\nInstall it using:\ninstall.packages('sensobol')"
    )
  }

  matrices <- as.character(matrices)
  order <- match.arg(order)
  type <- match.arg(type)

  if (!all(c("mcnode", "sample_space") %in% names(mctable))) {
    stop("mctable must contain columns 'mcnode' and 'sample_space'")
  }

  if (!"mc_func" %in% names(mctable) && "func" %in% names(mctable)) {
    mctable$mc_func <- mctable$func
  }

  spl <- split_mctable_for_sampling(mctable, mc_names = mc_names)
  mctable_in <- spl$mctable_in

  factors <- as.character(mctable_in$mcnode)
  sample_space <- as.character(mctable_in$sample_space)

  mc_func <- if ("mc_func" %in% names(mctable_in)) {
    as.character(mctable_in$mc_func)
  } else {
    rep(NA_character_, length(factors))
  }

  transforms <- if (
    isTRUE(transformation) && "transformation" %in% names(mctable_in)
  ) {
    as.character(mctable_in$transformation)
  } else {
    rep(NA_character_, length(factors))
  }

  # Drop constant nodes BEFORE generating matrices (each factor adds N rows
  # to the AB matrix).
  dropped <- character(0)
  if (isTRUE(drop_constant) && length(factors) > 0) {
    is_constant <- vapply(
      seq_along(factors),
      function(j) {
        is_constant_space(
          sample_space[j],
          mc_func = mc_func[j],
          transform = transforms[j],
          node_name = factors[j]
        )
      },
      logical(1)
    )

    if (any(is_constant)) {
      dropped <- factors[is_constant]
      keep <- !is_constant
      factors <- factors[keep]
      sample_space <- sample_space[keep]
      mc_func <- mc_func[keep]
      transforms <- transforms[keep]

      message(sprintf(
        "Dropped %d input(s) with no variation: %s",
        length(dropped),
        paste(dropped, collapse = ", ")
      ))
    }
  }

  p <- length(factors)
  if (p == 0) {
    stop(
      "No sampled factors: all nodes were excluded, constant, or have missing sample_space"
    )
  }

  # sensobol 1.1.6 interface:
  # sobol_matrices(matrices = c("A", "B", "AB"), N, params, order = "first", type = "QRN", ...)
  U <- sensobol::sobol_matrices(
    matrices = matrices,
    N = N,
    params = factors,
    order = order,
    type = type,
    ...
  )

  U <- as.matrix(U)

  # Clamp U for safety (qnorm(0/1) = +/-Inf)
  # NOTE: pmin/pmax can drop dimensions when there is a single column.
  eps <- 1e-12
  Uc <- pmin(1 - eps, pmax(eps, U))
  Uc <- matrix(Uc, nrow = nrow(U), ncol = ncol(U), dimnames = dimnames(U))

  X <- Uc

  for (j in seq_len(p)) {
    x_j <- qsample_space(
      Uc[, j],
      sample_space[j],
      mc_func = mc_func[j],
      node_name = factors[j]
    )
    x_j <- transform_sample_values(x_j, transforms[j], node_name = factors[j])

    if (!is.numeric(x_j) && !is.logical(x_j)) {
      stop(sprintf(
        "'%s' has a categorical sample_space; provide a numeric transformation",
        factors[j]
      ))
    }

    X[, j] <- as.numeric(x_j)
  }

  attr(X, "dropped") <- dropped
  X
}
