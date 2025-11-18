# Match Monte Carlo Nodes

Matches two mcnodes by:

1.  Group matching - Align nodes with same scenarios but different group
    order

2.  Scenario matching - Align nodes with same groups but different
    scenarios

3.  Null matching - Add missing groups across different scenarios

## Usage

``` r
mc_match(mcmodule, mc_name_x, mc_name_y, keys_names = NULL)
```

## Arguments

- mcmodule:

  Monte Carlo module

- mc_name_x:

  First node name

- mc_name_y:

  Second node name

- keys_names:

  Names of key columns

## Value

List containing matched nodes and combined keys (keys_xy)

## Examples

``` r
test_module <- list(
  node_list = list(
    node_x = list(
      mcnode = mcstoc(runif,
        min = mcdata(c(1, 2, 3), type = "0", nvariates = 3),
        max = mcdata(c(2, 3, 4), type = "0", nvariates = 3),
        nvariates = 3
      ),
      data_name = "data_x",
      keys = c("category")
    ),
    node_y = list(
      mcnode = mcstoc(runif,
        min = mcdata(c(5, 6, 7), type = "0", nvariates = 3),
        max = mcdata(c(6, 7, 8), type = "0", nvariates = 3),
        nvariates = 3
      ),
      data_name = "data_y",
      keys = c("category")
    )
  ),
  data = list(
    data_x = data.frame(
      category = c("A", "B", "C"),
      scenario_id = c("0", "0", "0")
    ),
    data_y = data.frame(
      category = c("B", "B", "B"),
      scenario_id = c("0", "1", "2")
    )
  )
)

result <- mc_match(test_module, "node_x", "node_y")
#> Group by: category
#> node_x prev dim: [1001, 1, 3], new dim: [1001, 1, 5], 0 null matches
#> node_y prev dim: [1001, 1, 3], new dim: [1001, 1, 5], 2 null matches
```
