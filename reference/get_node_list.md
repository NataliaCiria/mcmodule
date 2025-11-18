# Create Node List from Model Expression

Creates a list of nodes based on a given model expression, handling
input, output, and previous nodes with their properties and
relationships.

## Usage

``` r
get_node_list(
  exp,
  param_names = NULL,
  mctable = set_mctable(),
  data_keys = set_data_keys(),
  keys = NULL
)
```

## Arguments

- exp:

  An R expression containing model calculations

- param_names:

  Optional named vector for parameter renaming

- mctable:

  Reference table for mcnodes, defaults to set_mctable()

- data_keys:

  Data structure and keys, defaults to set_data_keys()

- keys:

  Optional explicit keys for the input data (character vector)

## Value

A list of class "mcnode_list" containing node information
