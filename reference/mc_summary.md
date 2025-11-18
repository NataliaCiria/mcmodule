# Compute summary statistics for an mcnode object

Compute summary statistics for an mcnode object

## Usage

``` r
mc_summary(
  mcmodule = NULL,
  mc_name = NULL,
  keys_names = NULL,
  data = NULL,
  mcnode = NULL,
  sep_keys = TRUE,
  digits = NULL
)
```

## Arguments

- mcmodule:

  An mcmodule object containing the node to summarize

- mc_name:

  Character string specifying the name of the mcnode in the module

- keys_names:

  Vector of column names to use as keys for grouping (default: NULL)

- data:

  Optional data frame containing the input data (default: NULL)

- mcnode:

  Optional mcnode object to summarize directly (default: NULL)

- sep_keys:

  Logical; if TRUE, keeps keys in separate columns (default: TRUE)

- digits:

  Integer indicating number of significant digits for rounding (default:
  NULL)

## Value

A data frame containing summary statistics with columns:

- mc_name: Name of the mcnode

- keys: Grouping variables (if sep_keys=FALSE) or individual key columns
  (if sep_keys=TRUE)

- Summary statistics including:

  - mean: Average value

  - sd: Standard deviation

  - Various quantiles (2.5%, 25%, 50%, 75%, 97.5%)

## Details

This function can be called in two ways:

1.  By providing an mcmodule and mc_name

2.  By providing data and mcnode directly

## Examples

``` r
# Use with mcmodule
summary_basic <- mc_summary(imports_mcmodule, "w_prev")

# Using custom keys and rounding
summary_custom <- mc_summary(imports_mcmodule, "w_prev",
  keys_names = c("origin"),
  digits = 3
)

# Use with data and mcnode
w_prev <- imports_mcmodule$node_list$w_prev$mcnode
summary_direct <- mc_summary(
  data = imports_data,
  mcnode = w_prev,
  sep_keys = FALSE
)
```
