# Match Monte Carlo Node with other data frame

Matches an mcnode with a data frame by:

1.  Group matching - Same scenarios but different group order

2.  Scenario matching - Same groups but different scenarios

3.  Null matching - Add missing groups across different scenarios

## Usage

``` r
mc_match_data(mcmodule, mc_name, data, keys_names = NULL)
```

## Arguments

- mcmodule:

  Monte Carlo module

- mc_name:

  Node name

- data:

  Data frame containing keys to match with

- keys_names:

  Names of key columns

## Value

List containing matched node, matched data and combined keys (keys_xy

## Examples

``` r
test_data  <- data.frame(pathogen=c("a","b"),
                         inf_dc_min=c(0.05,0.3),
                         inf_dc_max=c(0.08,0.4))
result<-mc_match_data(imports_mcmodule,"no_detect_a", test_data)
#> Group by: pathogen
#> no_detect_a prev dim: [1001, 1, 6], new dim: [1001, 1, 6], 0 null matches
#> test_data prev dim: [2, 3], new dim: [6, 4], 0 null matches
```
