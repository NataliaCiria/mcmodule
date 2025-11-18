# Generate Network Node Table for Visualization

Creates a formatted node table for visualization with visNetwork.
Includes styling and formatting for network visualization.

## Usage

``` r
visNetwork_nodes(
  mcmodule,
  variate = 1,
  color_pal = NULL,
  color_by = NULL,
  inputs = FALSE
)
```

## Arguments

- mcmodule:

  An mcmodule object containing the network structure

- variate:

  Integer specifying which variate to extract (default: 1)

- color_pal:

  Custom color palette for nodes (optional)

- color_by:

  Column name to determine node colors (optional)

- inputs:

  Include non-node inputs: data-sets, data-frames and columns (optional)

## Value

A data frame formatted for visNetwork with columns:

- id: Unique node identifier

- color: Node color based on type/category

- grouping: Module association

- expression: Node expression or type

- title: Hover text containing node details
