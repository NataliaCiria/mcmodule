# Get Nodes from Monte Carlo Module

Retrieves nodes from a Monte Carlo module and assigns them to the parent
environment

## Usage

``` r
get_mcmodule_nodes(mcmodule, mc_names = NULL, envir = parent.frame())
```

## Arguments

- mcmodule:

  An mcmodule or mcnode_list object

- mc_names:

  Optional vector of node names to retrieve

- envir:

  Environment where MC nodes will be created (default: parent.frame())

## Value

A subset of the node list containing requested nodes
