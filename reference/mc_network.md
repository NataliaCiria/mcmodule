# Create Interactive Network Visualization

Generates an interactive network visualization using visNetwork library.
The visualization includes interactive features for exploring model
structure and relationships.

## Usage

``` r
mc_network(
  mcmodule,
  variate = 1,
  color_pal = NULL,
  color_by = NULL,
  legend = FALSE,
  inputs = FALSE
)
```

## Arguments

- mcmodule:

  An mcmodule object

- variate:

  Integer specifying which variate to visualize (default: 1)

- color_pal:

  Custom color palette for nodes (optional)

- color_by:

  Column name to determine node colors (optional)

- legend:

  Show colors legend (optional)

- inputs:

  Show non-node inputs: data-sets, data-frames and columns (optional)

## Value

An interactive visNetwork object with features:

- Highlighting of connected nodes

- Node selection and filtering by module

- Directional arrows showing relationships

- Hierarchical layout

- Draggable nodes

## Examples

``` r
# \donttest{
network <- mc_network(mcmodule=imports_mcmodule)
# }
```
