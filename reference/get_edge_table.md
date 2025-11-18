# Generate Edge Table for Network Construction

Creates a data frame containing edge relationships between nodes in a
Monte Carlo module network. Each row represents a directed edge from one
node to another.

## Usage

``` r
get_edge_table(mcmodule, inputs = FALSE)
```

## Arguments

- mcmodule:

  An mcmodule object containing node relationships

- inputs:

  Include non-node inputs: data-sets, data-frames and columns (optional)

## Value

A data frame with columns node_from and node_to representing network
edges

## Examples

``` r
edge_table <- get_edge_table(imports_mcmodule)
```
