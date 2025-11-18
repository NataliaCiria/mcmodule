# Combine Two Modules

Combines two mcmodules into a single mcmodule by merging their data and
components.

## Usage

``` r
combine_modules(mcmodule_x, mcmodule_y)
```

## Arguments

- mcmodule_x:

  First module to combine

- mcmodule_y:

  Second module to combine

## Value

A combined mcmodule object

## Examples

``` r
module_x <- list(
  data = list(data_x = data.frame(x = 1:3)),
  node_list = list(
    node1 = list(type = "in_node"),
    node2 = list(type = "out_node")
  ),
  modules = c("module_x"),
  exp = quote({node2 <- node1 * 2})
)

module_y <- list(
  data = list(data_y = data.frame(y = 4:6)),
  node_list = list(node3 = list(type = "out_node")),
  modules = c("module_y"),
  exp = quote({node3 <- node1 + node2})
)

module_xy <- combine_modules(module_x, module_y)
```
