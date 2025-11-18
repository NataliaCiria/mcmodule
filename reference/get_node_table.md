# Generate Node Table for Network Construction

Creates a data frame containing node information from a Monte Carlo
module network. Includes node attributes, values, and relationships.

## Usage

``` r
get_node_table(mcmodule, variate = 1, inputs = FALSE)
```

## Arguments

- mcmodule:

  An mcmodule object containing node information

- variate:

  Integer indicating which variate to extract (default: 1)

- inputs:

  Include non-node inputs: data-sets, data-frames and columns (optional)

## Value

A data frame containing node information and attributes

## Examples

``` r
node_table <- get_node_table(imports_mcmodule)
```
