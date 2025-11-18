# Match and align keys between two datasets

Match and align keys between two datasets

## Usage

``` r
keys_match(x, y, keys_names = NULL)
```

## Arguments

- x:

  First dataset containing keys to match

- y:

  Second dataset containing keys to match

- keys_names:

  Names of columns to use as matching keys. If NULL, uses common columns

## Value

List containing:

- x:

  First dataset with group IDs

- y:

  Second dataset with group IDs

- xy:

  Matched datasets with aligned group and scenario IDs
