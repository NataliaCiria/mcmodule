# Aggregate Across Groups

Combines node values across specified grouping variables using different
aggregation methods. The aggregation method can be specified via
agg_func parameter:

- "prob": Combined probability assuming independence

- "sum": Sum of values

- "avg": Average of values

- NULL: defaults to "sum" if mc_name ends in "\_n", else defaults to
  "prob"

## Usage

``` r
agg_totals(
  mcmodule,
  mc_name,
  agg_keys = NULL,
  agg_suffix = NULL,
  prefix = NULL,
  name = NULL,
  summary = TRUE,
  keep_variates = FALSE,
  agg_func = NULL
)
```

## Arguments

- mcmodule:

  mcmodule object containing nodes and data

- mc_name:

  name of node to aggregate

- agg_keys:

  grouping variables for aggregation

- agg_suffix:

  Suffix for aggregated node name (default: "agg")

- prefix:

  Optional prefix for output node name - includes metadata as
  add_prefix() (default: NULL)

- name:

  Custom name for output node (optional)

- summary:

  whether to include summary statistics (default: TRUE)

- keep_variates:

  whether to preserve individual values (default: FALSE)

- agg_func:

  aggregation method ("prob", "sum", "avg", or NULL)

## Value

mcmodule with new aggregated node added

## Examples

``` r
imports_mcmodule <- agg_totals(
  imports_mcmodule, "no_detect_a",
  agg_keys = c("scenario_id", "pathogen")
)
#> 3 variates per group for no_detect_a
print(imports_mcmodule$node_list$no_detect_a_agg$summary)
#>           mc_name scenario_id pathogen      mean         sd       Min      2.5%
#> 1 no_detect_a_agg           0        a 0.3264681 0.01416266 0.2891882 0.2996985
#> 4 no_detect_a_agg           0        b 0.6610791 0.03083952 0.5904521 0.6087351
#>         25%       50%       75%     97.5%       Max  nsv Na's
#> 1 0.3160279 0.3266146 0.3365067 0.3530371 0.3632177 1001    0
#> 4 0.6354902 0.6612333 0.6865692 0.7119438 0.7183356 1001    0
```
