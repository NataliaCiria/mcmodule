# Create Monte Carlo Nodes from Data and Configuration Table

Creates Monte Carlo nodes (mcnodes) based on instructions provided in a
configuration table (mctable) and input variables from a dataframe.

## Usage

``` r
create_mcnodes(data, mctable = set_mctable(), envir = parent.frame())
```

## Arguments

- data:

  A data frame containing the input variables for creating Monte Carlo
  nodes

- mctable:

  A configuration table specifying MC node definitions. Must contain
  columns:

  - mcnode: Name of the Monte Carlo node

  - mc_func: Distribution function to use (if applicable)

  - transformation: Optional transformation to apply to input data

  - from_variable: Optional source variable name for transformation

- envir:

  Environment where MC nodes will be created (default: parent.frame())

## Value

No return value, creates MC nodes in the specified environment

## Examples

``` r
create_mcnodes(data = imports_data, mctable = imports_mctable)
```
