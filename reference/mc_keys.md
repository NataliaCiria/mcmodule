# Get Monte Carlo Node Keys

Extracts key columns from Monte Carlo node's associated data.

## Usage

``` r
mc_keys(mcmodule, mc_name, keys_names = NULL)
```

## Arguments

- mcmodule:

  Monte Carlo module containing nodes and data

- mc_name:

  Name of the node to extract keys from

- keys_names:

  Vector of column names to extract (optional)

## Value

Dataframe with scenario_id and requested key columns

## Examples

``` r
keys_df <- mc_keys(imports_mcmodule, "w_prev")
```
