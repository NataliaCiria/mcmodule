# Set or Get Monte Carlo Inputs Table

Manages a Monte Carlo inputs table in the global package environment by
either setting new data or retrieving the current table. The table
stores information about Monte Carlo nodes including their descriptions,
functions, dependencies, and sensitivity analysis settings.

## Usage

``` r
set_mctable(data = NULL)
```

## Arguments

- data:

  Optional data frame containing MC table information. Must contain an
  'mcnode' column. Other columns will be auto-filled if missing. If
  NULL, returns the current MC table.

## Value

- If data = NULL: Returns the current MC table

- If data provided: Sets the new MC table and returns invisibly

The table contains the following columns:

- mcnode - Character. Name of the Monte Carlo node (required)

- description - Character. Description of the parameter

- mc_func - Character. Probability distribution

- from_variable - Character. Variable name in the data table, if it is
  in a column with a name different from the mcnode

- transformation - Character. Transformation to be applied to the
  original column values

- sensi_analysis - Logical. Whether to include in sensitivity analysis

## Examples

``` r
# Get current MC table
current_table <- set_mctable()

# Set new MC table
mct <- data.frame(
  mcnode = c("h_prev", "w_prev"),
  description = c("Herd prevalence", "Within herd prevalence"),
  mc_func = c("runif", "runif"),
  sensi_analysis = c(TRUE, TRUE)
)
set_mctable(mct)
#> Warning: from_variable, transformation not specified
#> mctable set to mct
```
